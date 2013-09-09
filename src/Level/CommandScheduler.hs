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

type LevelState m = StateT Level m ()

data CommandScheduler m =
  CommandScheduler
  { _scheduler :: S.Scheduler Command (LevelState m)
  , _level     :: Level
  }
makeLenses ''CommandScheduler

runCommandScheduler :: (Monad m) => StateT (CommandScheduler m) m a -> Level -> m Level
runCommandScheduler s lvl = do
  cmdSched <- execStateT s (CommandScheduler S.empty lvl)
  return $ cmdSched ^. level

addCommand :: (Monad m, MonadState (CommandScheduler m) ms) => Command (LevelState m) -> ms ()
addCommand c = scheduler %= (execState $ S.add c)

executeGameStep :: (Functor m, Monad m) => StateT (CommandScheduler m) m ()
executeGameStep = do
  transformations <- zoom scheduler S.next
  assign level =<< lift . execStateT (sequence_ transformations) =<< use level
