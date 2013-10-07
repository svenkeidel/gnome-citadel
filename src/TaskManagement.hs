{-# LANGUAGE TemplateHaskell, FlexibleContexts, RankNTypes #-}
module TaskManagement ( TaskManager
                      , activeTaskQueue
                      , inactiveTaskQueue
                      , numberOfTasks
                      , hasTask
                      , addTask
                      , addReachableTask
                      , addUnreachableTask
                      , getTask
                      ) where

import Control.Lens.TH
import Control.Lens.Operators
import Control.Lens (each,findOf)
import qualified Control.Lens.Getter as LG

import Data.Default

import qualified Data.Sequence as DS
import qualified Data.Foldable as DF

import Queue
import Task
import Types
import Utils

data TaskManager = TaskManager { _activeTaskQueue :: Queue Task
                               , _inactiveTaskQueue :: Queue Task
                               } deriving (Show,Eq)
makeLenses ''TaskManager

instance Default TaskManager where
  def = TaskManager DS.empty DS.empty

numberOfTasks :: TaskManager -> Int
numberOfTasks tm = numberOfActiveTasks tm + numberOfInactiveTasks tm

numberOfActiveTasks :: TaskManager -> Int
numberOfActiveTasks tm = DS.length $ tm ^. activeTaskQueue

numberOfInactiveTasks :: TaskManager -> Int
numberOfInactiveTasks tm = DS.length $ tm ^. inactiveTaskQueue

hasTask :: Identifier -> TaskManager -> Bool
hasTask tId tm = DF.any match (tm ^. activeTaskQueue) ||
                 DF.any match (tm ^. inactiveTaskQueue)
  where
    match t = t ^. taskId == tId

addReachableTask :: Task -> TaskManager -> TaskManager
addReachableTask task = activeTaskQueue %~ enqueue task

addUnreachableTask :: Task -> TaskManager -> TaskManager
addUnreachableTask task = inactiveTaskQueue %~ enqueue task

addTask :: Bool -> Task -> TaskManager -> TaskManager
addTask isReachable task = if isReachable
                              then addReachableTask task
                              else addUnreachableTask task

getTask :: Identifier -> TaskManager -> Maybe Task
getTask tId manager = foundTask
  where
    foundTask :: Maybe Task
    foundTask = useFirst [ findTaskInQueue activeTaskQueue
                         , findTaskInQueue inactiveTaskQueue
                         ]

    findTaskInQueue :: LG.Getter TaskManager (Queue Task) -> Maybe Task
    findTaskInQueue queue = findTask manager queue (matchId tId)

matchId :: Identifier -> Task -> Bool
matchId tId task = tId == task ^. taskId

findTask :: TaskManager -> LG.Getter TaskManager (Queue Task) -> (Task -> Bool) -> Maybe Task
findTask manager queue p = findOf (queue . each) p manager
