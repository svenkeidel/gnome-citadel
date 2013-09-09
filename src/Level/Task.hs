{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Level.Task ( createTask
                  , numberOfTasks
                  , hasTask
                  , getTask
                  ) where

import Control.Lens hiding (Level)
import qualified Control.Lens.Getter as LG
import Control.Monad.State
import Control.Applicative

import qualified Data.Monoid as DM
import qualified Data.Foldable as F
import qualified Data.Sequence as S
import qualified Data.Map as M

import Level
import Task
import Queue

createTask :: MonadState Level m => Coord -> TaskType -> m Task
createTask coord tType = do
  currentLevel <- get
  nextId <- freshId
  let task = Task nextId coord tType
      targetQueue = if isReachable coord currentLevel
                       then activeTaskQueue
                       else inactiveTaskQueue
  targetQueue %= enqueue task
  idToCoord %= M.insert nextId coord
  return task

hasTask :: Identifier -> Level -> Bool
hasTask tId lvl = F.any match (lvl ^. activeTaskQueue) ||
                  F.any match (lvl ^. inactiveTaskQueue)
  where
    match t = t ^. taskId == tId

getTask :: Identifier -> Level -> Maybe (Coord,Task)
getTask tId lvl = (,) <$> taskCoordinate <*> foundTask
  where
    taskCoordinate :: Maybe Coord
    taskCoordinate = M.lookup tId (lvl ^. idToCoord)

    foundTask :: Maybe Task
    foundTask = useFirst [ findTaskInQueue activeTaskQueue
                         , findTaskInQueue inactiveTaskQueue
                         ]

    findTaskInQueue :: LG.Getter Level (Queue Task) -> Maybe Task
    findTaskInQueue queue = findTask lvl queue (matchId tId)

matchId :: Identifier -> Task -> Bool
matchId tId task = tId == task ^. taskId

findTask :: Level -> LG.Getter Level (Queue Task) -> (Task -> Bool) -> Maybe Task
findTask lvl queue p = F.find p $ lvl ^. queue

numberOfTasks :: Level -> Int
numberOfTasks lvl = numberOfActiveTasks lvl + numberOfInactiveTasks lvl

numberOfActiveTasks :: Level -> Int
numberOfActiveTasks lvl = S.length $ lvl ^. activeTaskQueue

numberOfInactiveTasks :: Level -> Int
numberOfInactiveTasks lvl = S.length $ lvl ^. inactiveTaskQueue

isReachable :: Coord -> Level -> Bool
isReachable = const $ const False

useFirst:: F.Foldable t => t (Maybe a) -> Maybe a
useFirst = DM.getFirst . F.foldMap DM.First
