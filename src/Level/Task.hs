{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Level.Task ( createTask
                  , numberOfTasks
                  , hasTask
                  , getTask
                  ) where

import Control.Lens hiding (Level)
import Control.Monad.State
import Control.Applicative

import qualified Data.Map as DM

import qualified TaskManagement as T

import Level
import Task

createTask :: MonadState Level m => Coord -> TaskType -> m Task
createTask coord tType = do
  currentLevel <- get
  nextId <- freshId
  let task = Task nextId coord tType
  taskManager %= T.addTask (isReachable coord currentLevel) task
  idToCoord %= DM.insert nextId coord
  return task

hasTask :: Identifier -> Level -> Bool
hasTask tId lvl = T.hasTask tId $ lvl ^. taskManager

getTask :: Identifier -> Level -> Maybe (Coord,Task)
getTask tId lvl = (,) <$> taskCoordinate <*> foundTask
  where
    taskCoordinate :: Maybe Coord
    taskCoordinate = DM.lookup tId (lvl ^. idToCoord)

    foundTask :: Maybe Task
    foundTask = T.getTask tId (lvl ^. taskManager)

numberOfTasks :: Level -> Int
numberOfTasks lvl = T.numberOfTasks $ lvl ^. taskManager
