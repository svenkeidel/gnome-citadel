{-# LANGUAGE TemplateHaskell, FlexibleContexts, RankNTypes #-}
module TaskManagement ( TaskManager
                      , taskManager
                      , nextFreeId
                      , reachableBy
                      , active
                      , inactive
                      , taskAssignment
                      , addTask
                      , canBeDoneBy
                      , bestForTheJob
                      , assignTasks
                      , assignTo
                      ) where

import Control.Lens(zoom,contains)
import Control.Lens.TH
import Control.Lens.Operators
import Control.Monad.State
import Control.Monad.Error
import Control.Applicative

import Data.Default
import Data.Maybe(listToMaybe)
import Data.List(sortBy)
import Data.Ord(comparing)
import qualified Data.Set as Set
import qualified Data.Map as M
import qualified Data.Foldable as F

import Actor(Actor)
import qualified Actor
import Counter
import Task
import Level hiding (actors, isReachable, _nextFreeId)
import qualified Level
import Level.Scheduler(CommandScheduler)
import qualified Level.Scheduler as Scheduler

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
                               }
makeLenses ''TaskManager

taskManager :: TaskManager
taskManager = TaskManager { _inactive       = def
                          , _active         = def
                          , _taskAssignment = def
                          , _reachableBy    = error "TaskManager.reachableBy undefined"
                          , _nextFreeId     = def
                          }

addTaskE :: (Identifier Task -> Either e Task) -> TaskManager -> Either e TaskManager
addTaskE t tm = let (task,tm') = flip runState tm $ t <$> zoom nextFreeId freshId
                in case task of
                  Left e -> Left e
                  Right task' -> Right $ tm' & inactive %~ Set.insert task'

-- | Adds a task to the task manager. The task is initial inactive.
addTask :: (Identifier Task -> Task) -> TaskManager -> TaskManager
addTask t tm = flip execState tm $ do
  task <- t <$> zoom nextFreeId freshId
  inactive %= Set.insert task

-- | Determines if the given task can be done by the given actor
canBeDoneBy :: Task -> Actor -> Level -> TaskManager -> Bool
canBeDoneBy task actor lvl tm = hasAbility && not busy && reachable
  where
    busy       = M.notMember (actor ^. Actor.id) (tm ^. taskAssignment)
    hasAbility = actor ^. Actor.abilities . contains (task ^. taskType)
    reachable  = _reachableBy tm lvl task actor

-- | Searches from a list of actors the actor that is best suited for
-- the task. This implies that the actor has the proper abilities and
-- no other actor is physically nearer located to the task (this does
-- not mean that he has the shortest path to the task).
bestForTheJob :: Task -> [Actor] -> Level -> TaskManager -> Maybe Actor
bestForTheJob task actors lvl tm =
  listToMaybe $ sortBy distanceToTask
              $ filter (\actor -> canBeDoneBy task actor lvl tm) actors
  where
    distanceToTask = comparing $ \actor ->
      distance (lvl ^. coordOf actor) (task ^. Task.target)

-- | Looks through the list of inactive tasks and tries to
-- automatically assign those to idle dwarves.
assignTasks :: Level -> (CommandScheduler, TaskManager) -> (CommandScheduler, TaskManager)
assignTasks lvl (cmdScheduler0, tm0) = F.foldl go (cmdScheduler0, tm0) (tm0 ^. inactive)
  where
    go :: (CommandScheduler, TaskManager) -> Task -> (CommandScheduler, TaskManager)
    go (cmdScheduler, tm) task =
      case bestForTheJob task (M.elems $ lvl ^. Level.actors) lvl tm of
        Just actor -> assignTo task actor (cmdScheduler, tm)
        Nothing    -> (cmdScheduler, tm)

-- | Assign the given task to the given actor and add the appropriate
-- command to the command scheduler.
assignTo :: Task -> Actor -> (CommandScheduler, TaskManager) -> (CommandScheduler, TaskManager)
assignTo task actor (cmdScheduler, tm) = (cmdScheduler', tm')
  where
    tm' = tm & inactive       %~ Set.delete task
             & active         %~ Set.insert task
             & taskAssignment %~ M.insert (actor ^. Actor.id) (task ^. Task.id)
    cmdScheduler' = Scheduler.addCommand (Task._command task actor) cmdScheduler
