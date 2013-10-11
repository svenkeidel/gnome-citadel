{-# LANGUAGE TemplateHaskell, FlexibleContexts, RankNTypes #-}
module TaskManagement ( TaskManager
                      , taskManager
                      , runTaskManager
                      , activeTaskQueue
                      , inactiveTaskQueue
                      , reachable
                      , numberOfTasks
                      , hasTask
                      , addTask
                      , getTask
                      ) where

import Control.Lens (each,findOf,zoom)
import Control.Lens.TH
import Control.Lens.Operators
import qualified Control.Lens.Getter as LG

import Control.Applicative
import Control.Monad.State

import Data.Default
import qualified Data.Sequence as DS
import qualified Data.Foldable as DF

import Counter
import Queue
import Task
import Types
import Utils
import Level.Scheduler
import Level hiding (isReachable, _nextFreeId)

data TaskManager = TaskManager { _activeTaskQueue :: Queue Task
                               , _inactiveTaskQueue :: Queue Task
                               , _reachable :: Level -> Task -> Bool
                               , _nextFreeId :: Counter
                               , _cmdScheduler :: CommandScheduler
                               }
makeLenses ''TaskManager

taskManager :: Level -> TaskManager
taskManager lvl = TaskManager { _activeTaskQueue   = DS.empty
                              , _inactiveTaskQueue = DS.empty
                              , _reachable         = error "TaskManager.isReachable is undefined"
                              , _nextFreeId        = def
                              , _cmdScheduler      = commandScheduler lvl
                              }

runTaskManager :: (Monad m) => StateT TaskManager m a -> Level -> m Level
runTaskManager s lvl = do
  tskMgr <- execStateT s $ taskManager lvl
  return $ tskMgr ^. cmdScheduler . level

numberOfTasks :: LG.Getter TaskManager Int
numberOfTasks = LG.to $ \tm ->
  tm ^. numberOfActiveTasks + tm ^. numberOfInactiveTasks

numberOfActiveTasks :: LG.Getter TaskManager Int
numberOfActiveTasks = LG.to $ \tm ->
  DS.length $ tm ^. activeTaskQueue

numberOfInactiveTasks :: LG.Getter TaskManager Int
numberOfInactiveTasks = LG.to $ \tm ->
  DS.length $ tm ^. inactiveTaskQueue

hasTask :: Identifier -> LG.Getter TaskManager Bool
hasTask tId = LG.to $ \tm ->
  DF.any match (tm ^. activeTaskQueue) ||
  DF.any match (tm ^. inactiveTaskQueue)
  where
    match t = t ^. taskId == tId

isReachable :: Task -> LG.Getter TaskManager Bool
isReachable task = LG.to $ \tm ->
  let lvl = tm ^. cmdScheduler . level
  in _reachable tm lvl task

addTask :: (Functor m, Monad m) => (Identifier -> Task) -> StateT TaskManager m ()
addTask t = do
  task <- t <$> zoom nextFreeId freshId
  isReachable' <- LG.use $ isReachable task
  if isReachable'
    then addReachableTask task
    else addUnreachableTask task
  where
    -- reachable and unreachable is an implementation detail. The user
    -- of this function should not interface with that
    addReachableTask   task' = activeTaskQueue   %= enqueue task'
    addUnreachableTask task' = inactiveTaskQueue %= enqueue task'

getTask :: Identifier -> LG.Getter TaskManager (Maybe Task)
getTask tId = LG.to foundTask
  where
    foundTask :: TaskManager -> Maybe Task
    foundTask tm = useFirst [ tm ^. findTaskInQueue activeTaskQueue
                            , tm ^. findTaskInQueue inactiveTaskQueue
                            ]

    findTaskInQueue :: LG.Getter TaskManager (Queue Task) -> LG.Getter TaskManager (Maybe Task)
    findTaskInQueue queue = LG.to $ \tm -> findTask tm queue (matchId tId)

matchId :: Identifier -> Task -> Bool
matchId tId task = tId == task ^. taskId

findTask :: TaskManager -> LG.Getter TaskManager (Queue Task) -> (Task -> Bool) -> Maybe Task
findTask manager queue p = findOf (queue . each) p manager
