{-# LANGUAGE TemplateHaskell, FlexibleContexts, RankNTypes #-}
module TaskManagement ( TaskManager
                      , taskManager
                      , nextFreeId
                      , reachableBy
                      , active
                      , inactive
                      , taskAssignment
                      ) where

import Control.Lens.TH
import Control.Lens.Operators

import Data.Default
import qualified Data.Set as Set
import qualified Data.Map as M

import Actor(Actor)
import Counter
import Task
-- import Types
-- import Utils
import Level.Scheduler hiding (executeGameStep)
import qualified Level.Scheduler as CS
import Level hiding (isReachable, _nextFreeId)

-- | Lifecycle of a task
--
-- @
--                                new task
--                                    |
--                                    v
--                            +---------------+
--                            |    inactive   |
--                            +---------------+
--  dwarf has suitable ability, can  | ^ e.g. the dwarf with the suitable
--  reach the task target and has    | | ability died or the task target
--  is not busy with another task    v | has become unreachable
--                            +---------------+
--                            |    active     |
--                            +---------------+
--  dwarf succsessfully completed     |
--  the task                          v
--                            +---------------+
--                            |   completed   |
--                            +---------------+
-- @
data TaskManager = TaskManager { _inactive :: Set.Set Task
                               , _active :: Set.Set Task
                               , _reachableBy :: Level -> Task -> Actor -> Bool
                               , _taskAssignment :: M.Map (Identifier Actor) (Identifier Task)
                               , _nextFreeId :: Counter
                               , _cmdScheduler :: CommandScheduler
                               }
makeLenses ''TaskManager

instance Show TaskManager where
  show tm = "TaskManager { "
         ++ "commandScheduler = " ++ show (tm ^. cmdScheduler)
         ++ " }"

taskManager :: TaskManager
taskManager = TaskManager { _inactive       = Set.empty
                              , _active         = Set.empty
                              , _taskAssignment = M.empty
                              , _reachableBy    = error "TaskManager.reachableBy undefined"
                              , _nextFreeId     = def
                              , _cmdScheduler   = CS.empty
                              }
{-
runTaskManager :: (Monad m) => StateT TaskManager m a -> Level -> m Level
runTaskManager s lvl = do
  tskMgr <- execStateT s $ taskManager lvl
  return $ tskMgr ^. level


level :: Lens' TaskManager Level
level = cmdScheduler . CS.level


addTask :: (Functor m, Monad m) => (Identifier Task -> Task) -> StateT TaskManager m ()
addTask t = do
  task <- t <$> zoom nextFreeId freshId
  inactive %= Set.insert task

numberOfTasks :: LG.Getter TaskManager Int
numberOfTasks = LG.to $ \tm ->
  tm ^. numberOfActiveTasks + tm ^. numberOfInactiveTasks

numberOfActiveTasks :: LG.Getter TaskManager Int
numberOfActiveTasks = LG.to $ \tm ->
  DS.length $ tm ^. activeTaskQueue

numberOfInactiveTasks :: LG.Getter TaskManager Int
numberOfInactiveTasks = LG.to $ \tm ->
  DS.length $ tm ^. inactiveTaskQueue

hasTask :: Identifier Task -> LG.Getter TaskManager Bool
hasTask tId = LG.to $ \tm ->
  DF.any match (tm ^. activeTaskQueue) ||
  DF.any match (tm ^. inactiveTaskQueue)
  where
    match t = t ^. Task.id == tId


getTask :: Identifier Task -> LG.Getter TaskManager (Maybe Task)
getTask tId = LG.to foundTask
  where
    foundTask :: TaskManager -> Maybe Task
    foundTask tm = useFirst [ tm ^. findTaskInQueue activeTaskQueue
                            , tm ^. findTaskInQueue inactiveTaskQueue
                            ]

    findTaskInQueue :: LG.Getter TaskManager (Queue Task) -> LG.Getter TaskManager (Maybe Task)
    findTaskInQueue queue = LG.to $ \tm -> findTask tm queue (matchId tId)

matchId :: Identifier Task -> Task -> Bool
matchId tId task = tId == task ^. Task.id

findTask :: TaskManager -> LG.Getter TaskManager (Queue Task) -> (Task -> Bool) -> Maybe Task
findTask manager queue p = findOf (queue . each) p manager

assignTasks :: StateT TaskManager m ()
assignTasks = undefined


inactiveToActive :: TaskManager -> TaskManager
inactiveToActive tm =
  foldlOf (inactive.each) go tm
  where
    go tm task =
      maybe tm
            (execState . assignTo task)
            (bestForTheJob task (tm ^. level . actors . each) tm)
      where
        assignTo task actor = do
          taskAssignment    %= M.insert (actor^.id) (task^.id)
          zoom cmdScheduler (addCommand $ task ^. Task.command)

executeGameStep :: (Functor m, Monad m) => StateT TaskManager (ErrorT LevelError m) ()
executeGameStep = zoom cmdScheduler CS.executeGameStep

isReachableBy :: Task -> Actor -> TaskManager -> Bool
isReachableBy task actor tm =
  let lvl = tm ^. level
  in _reachableBy tm lvl task actor

busy :: Actor -> LG.Getter TaskManager Bool
busy actor = LG.to $ \tm ->
  M.notMember (actor ^. Actor.id) (tm ^. taskAssignment)


bestForTheJob :: Task -> [Actor] -> TaskManager -> Maybe Actor
bestForTheJob task actors tm =
  listToMaybe $ sortByDistanceToTarget
              $ filter (\actor -> canBeDoneBy task actor tm) actors
  where
    sortByDistanceToTarget :: [Actor] -> [Actor]
    sortByDistanceToTarget actors =
      sortBy (distanceToTask $ tm ^. level) actors
      where
        distanceToTask lvl = comparing $ \actor ->
          distance (lvl ^. coordOf actor) (task ^. Task.target)

    canBeDoneBy :: Task -> Actor -> Bool
    canBeDoneBy task actor =

      hasAbility actor
      &&
      notBusy actor
      &&
      isReachableBy task actor tm

    notBusy actor = not $ tm ^. busy actor
    hasAbility actor = actor ^. Actor.abilities . contains (task ^. taskType)
-}
