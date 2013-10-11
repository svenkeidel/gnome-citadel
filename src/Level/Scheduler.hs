{-# LANGUAGE TemplateHaskell, RankNTypes, FlexibleContexts, ScopedTypeVariables #-}
module Level.Scheduler ( CommandScheduler
                       , level
                       , commandScheduler
                       , runCommandScheduler
                       , addCommand
                       , addCommandT
                       , executeGameStep
                       ) where

import Control.Lens ((^.),(%=),assign,zoom,use)
import Control.Lens.TH
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error

import Data.Functor.Identity

import Unfold
import Level
import Level.Transformation (LevelTrans,LevelError)
import Level.Command (Command,CommandT,runCommandT)

import qualified Scheduler as S

data CommandScheduler =
  CommandScheduler
  { _scheduler :: S.Scheduler Unfold (LevelTrans Identity)
  , _level     :: Level
  }
makeLenses ''CommandScheduler

commandScheduler :: Level -> CommandScheduler
commandScheduler = CommandScheduler S.empty

-- | Performs a sequence of transformations based on commands on a level.
runCommandScheduler :: (Monad m) => StateT CommandScheduler m a -> Level -> m Level
runCommandScheduler s lvl = do
  cmdSched <- execStateT s $ commandScheduler lvl
  return $ cmdSched ^. level

-- | Adds a command to a schedule. The command is not executed immediately.
addCommand :: (Monad m, MonadState CommandScheduler m) => Command -> m ()
addCommand c = scheduler %= execState (S.add c)

addCommandT :: (Monad m, Show e, MonadState CommandScheduler m)
            => CommandT (ReaderT Level (ErrorT e m)) -> m ()
addCommandT c = do
  lvl <- use level
  c' <- runErrorT
      $ flip runReaderT lvl
      $ runCommandT c
  addCommand $ case c' of
    Left e    -> return $ throwError $ strMsg (show e)
    Right c'' -> c''

-- | Applies the upcomming sequence of commands to the level. Lifts
-- all effects into the base monad of the state transformer.
executeGameStep :: (Functor m, Monad m) => StateT CommandScheduler (ErrorT LevelError m) ()
executeGameStep = do
  transformations <- zoom scheduler S.next
  assign level =<< lift
                 . ErrorT
                 . return
                 . runIdentity
                 . runErrorT
                 . execStateT (sequence_ transformations)
               =<< use level
