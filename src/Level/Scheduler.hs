{-# LANGUAGE TemplateHaskell, RankNTypes, FlexibleContexts, ScopedTypeVariables, ViewPatterns #-}
module Level.Scheduler ( CommandScheduler (CommandScheduler)
                       , empty
                       , addCommand
                       , addCommandT
                       , executeGameStep
                       ) where

import Control.Lens ((^.),(%~))
import Control.Lens.TH
import Control.Monad((>=>))

import Unfold
import Level
import Level.Transformation (LevelTrans,LevelError)
import Level.Command (Command,CommandT,runCommandT)

import qualified Scheduler as S

newtype CommandScheduler = CommandScheduler { _scheduler :: S.Scheduler Unfold LevelTrans }
makeLenses ''CommandScheduler

instance Show CommandScheduler where
  show _ = "TODO: implement show instance"

empty :: CommandScheduler
empty = CommandScheduler S.empty

-- | Adds a command to a schedule. The command is not executed immediately.
addCommand :: Command -> CommandScheduler -> CommandScheduler
addCommand command = scheduler %~ S.add command

addCommandT :: Monad m => CommandT m -> CommandScheduler -> m CommandScheduler
addCommandT c cmdScheduler = do
  c' <- runCommandT c
  return $ addCommand c' cmdScheduler

-- | Applies the upcomming sequence of commands to the level. Lifts
-- all effects into the base monad of the state transformer.
executeGameStep :: (Level, CommandScheduler) -> Either LevelError (Level, CommandScheduler)
executeGameStep (lvl, cmdScheduler) = do
  -- Kleisli Arrows (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)
  -- used to combine all level transformations into one single transformation
  -- foldl (>=>) return transformation :: Level -> Either LevelError Level
  lvl' <- foldl (>=>) return transformations $ lvl :: Either LevelError Level
  return (lvl', cmdScheduler')
  where
    (transformations, CommandScheduler -> cmdScheduler') = S.next (cmdScheduler ^. scheduler)
