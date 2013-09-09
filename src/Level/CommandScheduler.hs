{-# LANGUAGE TemplateHaskell, RankNTypes, FlexibleContexts, ScopedTypeVariables #-}
module Level.CommandScheduler ( CommandScheduler
                              , level
                              , runCommandScheduler
                              , addCommand
                              , executeGameStep
                              ) where

import Control.Lens ((^.),(%=),assign,zoom,use)
import Control.Lens.TH
import Control.Monad.State

import Level
import Level.Commands

import qualified Scheduler as S

data CommandScheduler m =
  CommandScheduler
  { _scheduler :: S.Scheduler Command (LevelTrans m)
  , _level     :: Level
  }
makeLenses ''CommandScheduler

-- | Performs a sequence of transformations based on commands on a level.
runCommandScheduler :: (Monad m) => StateT (CommandScheduler m) m a -> Level -> m Level
runCommandScheduler s lvl = do
  cmdSched <- execStateT s (CommandScheduler S.empty lvl)
  return $ cmdSched ^. level

-- | Adds a command to a schedule. The command is not executed immediately.
addCommand :: (Monad m, MonadState (CommandScheduler m) ms) => LevelCommand m -> ms ()
addCommand c = scheduler %= execState (S.add c)

-- | Applies the upcomming sequence of commands to the level. Lifts
-- all effects into the base monad of the state transformer.
executeGameStep :: (Functor m, Monad m) => StateT (CommandScheduler m) m ()
executeGameStep = do
  transformations <- zoom scheduler S.next
  assign level =<< lift . execStateT (sequence_ transformations) =<< use level
