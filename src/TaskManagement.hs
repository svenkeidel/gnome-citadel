{-# LANGUAGE TemplateHaskell, FlexibleContexts, RankNTypes, ViewPatterns #-}
module TaskManagement ( TaskManager
                      , AbortedTask (AbortedTask)
                      , Reachable(..)
                      , empty
                      , reachableBy
                      , calculateReachable
                      , reachable
                      , active
                      , inactive
                      , taskAssignment
                      , addTask
                      , addTaskE
                      , canBeDoneBy
                      , assignTasks
                      , idleActors
                      , assignTo
                      , isAssignedTo
                      , executeGameStep
                      ) where

import           Control.Coroutine
import           Control.Coroutine.AwaitYield
import           Control.Lens (contains,set,preview)
import           Control.Lens.Getter (view)
import           Control.Lens.Operators
import           Control.Lens.TH
import           Control.Monad (guard)

import           Data.Default
import           Data.Function (on)
import           Data.Functor.Identity
import           Data.List (sortBy,(\\))
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as Set

import           Actor (Actor)
import qualified Actor
import           Control.DeepSeq
import           Counter
import qualified Level
import           Level hiding (isReachable, actors)
import           Path (floodUntil,ContinueFlooding(..))
import           Task (Task,ActiveTask(..),TaskStatus(..))
import qualified Task
import           Utils ((^->))

data Reachable = Reachable | Unreachable deriving (Eq,Show)

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
                               , _active :: [ActiveTask]
                               , _reachableBy :: Task -> Actor -> Reachable
                               , _taskAssignment :: M.Map (Identifier Actor) (Identifier Task)
                               }
makeLenses ''TaskManager

instance NFData TaskManager where
  rnf (TaskManager ias as _ assigns) = rnf (ias,as,assigns)

instance Eq TaskManager where
  tm1 == tm2 = ((==) `on` view inactive) tm1 tm2 &&
               ((==) `on` view active) tm1 tm2 &&
               ((==) `on` view taskAssignment) tm1 tm2

instance Show TaskManager where
  show (TaskManager inactiv activ _ assignments) =
    "TaskManager { _inactive = " ++ show inactiv ++ ", "
      ++ "_active = " ++ show activ ++ ", "
      ++ "_reachableBy = undefined" ++ ", "
      ++ "_taskAssignment = " ++ show assignments ++ "}"

empty :: TaskManager
empty = TaskManager { _inactive       = def
                    , _active         = def
                    , _taskAssignment = def
                    , _reachableBy    = undefined
                    }

addTaskE :: Either e Task -> TaskManager -> Either e TaskManager
addTaskE task tm = case task of
                     Left e -> Left e
                     Right task' -> Right $ tm & inactive %~ Set.insert task'

-- | Adds a task to the task manager. The task is initial inactive.
addTask :: Task -> TaskManager -> TaskManager
addTask task tm = tm & inactive %~ Set.insert task

-- | Determines if the given task can be done by the given actor
canBeDoneBy :: TaskManager -> Actor -> Task -> Bool
canBeDoneBy tm actor task = hasAbility && not busy && isReachable == Reachable
  where
    busy       = M.member (actor ^. Actor.id) (tm ^. taskAssignment)
    hasAbility = actor ^. Actor.abilities . contains (task ^. Task.taskType)
    isReachable  = _reachableBy tm task actor

toIsReachable :: Bool -> Reachable
toIsReachable True = Reachable
toIsReachable False = Unreachable

type ReachableRelation = Set (Identifier Actor,Identifier Task)

reachable :: ReachableRelation -> Task -> Actor -> Reachable
reachable reachableRel task actor = actor `canReach` task
  where actor' `canReach` task' =
          toIsReachable $ Set.member (actor' ^. Actor.id, task' ^. Task.id) reachableRel

calculateReachable :: TaskManager -> Level -> ReachableRelation
calculateReachable tm lvl = go Set.empty idle
  where go :: Set (Identifier Actor,Identifier Task) -> [Identifier Actor] -> Set (Identifier Actor,Identifier Task)
        go acc [] = acc
        go acc (a:as) = go acc' (as \\ actors)
          where (actors,tasks) = floodFill lvl (tm ^. inactive) a
                acc' = foldr Set.insert acc [ (a',t) | a' <- actors, t <- tasks ]
        idle :: [Identifier Actor]
        idle = map (^. Actor.id) (idleActors lvl tm)


floodFill :: Level -> Set Task -> Identifier Actor -> ([Identifier Actor],[Identifier Task])
floodFill lvl ts actorId = (Set.toList actors,Set.toList tasks)
  where coords :: Set Coord
        coords = M.keysSet $ maybe M.empty (floodUntil (const Continue) (lvl ^-> walkable)) (preview (coordOf actorId) lvl)

        actors :: Set (Identifier Actor)
        actors = Set.foldl' (\acc coord -> Set.fromList (map (view Actor.id) (actorsAt lvl coord)) `Set.union` acc) Set.empty coords

        tasks :: Set (Identifier Task)
        tasks = Set.map (view Task.id) $ Set.filter (\t -> view Task.target t `Set.member` coords) ts

assignTasks :: Level -> TaskManager -> TaskManager
assignTasks lvl tm00 = chooseAssignments tm0 possibleAssignments
  where tm0 = set reachableBy (reachable reachableRel) tm00
        reachableRel = calculateReachable tm00 lvl
        chooseAssignments tm []             = tm
        chooseAssignments tm ((a,t):tuples) =
          let tm' = assignTo t a tm
          in chooseAssignments tm' $ filter (\(a',t') -> not (a == a' || t == t')) tuples
        possibleAssignments = sortBy (compareDistanceToTask) $ do
          idleActor <- idleActors lvl tm0
          task <- Set.toList . view inactive $ tm0
          guard $ canBeDoneBy tm0 idleActor task
          return (idleActor, task)
        helper i = preview (coordOf i) lvl
        compareDistanceToTask :: (Actor, Task) -> (Actor, Task) -> Ordering
        compareDistanceToTask (helper -> Just a1,view Task.target -> t1) (helper -> Just a2,view Task.target -> t2) =
          distance a1 t1 `compare` distance a2 t2
        compareDistanceToTask (helper -> Nothing,_) (helper -> Just _,_) = LT
        compareDistanceToTask (helper -> Just _,_)  (helper -> Nothing,_) = GT
        compareDistanceToTask (_,_) (_,_) = EQ

idleActors :: Level -> TaskManager -> [Actor]
idleActors lvl tm = actors
  where
    isBusy :: Identifier Actor -> Bool
    isBusy actor = M.member actor $ tm ^. taskAssignment
    actors = M.foldlWithKey' (\as k a -> if isBusy k then as else a :as) [] (lvl ^. Level.actors)

-- | Assign the given task to the given actor and add the appropriate
-- command to the command scheduler.
assignTo :: Task -> Actor -> TaskManager -> TaskManager
assignTo task actor
  = (inactive       %~ Set.delete task)
  . (active         %~ insert (ActiveTask task $ Task._command task actor))
  . (taskAssignment %~ M.insert (actor ^. Actor.id) (task ^. Task.id))
  where
    insert = (:)

unassignTask :: Task -> TaskManager -> TaskManager
unassignTask task = taskAssignment %~ M.filter (/= task ^. Task.id)

isAssignedTo :: Task -> Actor -> TaskManager -> Bool
isAssignedTo t a tm = M.lookup (a ^. Actor.id) (tm ^. taskAssignment) == Just (t ^. Task.id)

data AbortedTask = AbortedTask Task String
    deriving (Show)

instance NFData AbortedTask where
  rnf (AbortedTask t s) = rnf (t,s)

executeGameStep :: Level -> TaskManager -> ([AbortedTask], Level, TaskManager)
executeGameStep lvl0 tm0 = let (aborted,lvl',tm') = foldr go ([],lvl0,tm0 & active .~ []) (tm0 ^. active)
                           in (aborted, lvl', assignTasks lvl' tm')
  where
    go (ActiveTask task c) (err,lvl,tm) =
      case runIdentity (resume c) of
        Right () -> (err,lvl,unassignTask task tm)
        Left (Await f) -> go (ActiveTask task (f lvl)) (err,lvl,tm)
        Left (Yield status c') ->
          case status of
            -- simply drop the task.
            CannotBeCompleted s -> (AbortedTask task s : err,lvl, tm)

            -- drop the current state and readd the task as inactive.
            Reschedule          -> (err,lvl, addTask task tm)

            -- update the task state in the manager and the modified level.
            InProgress lvl'     -> (err,lvl', tm & active %~ insert (ActiveTask task c'))


        {-Done               -> (err,lvl, unassignTask task tm)-}
        {-Yield trans state' ->-}
          {-case trans lvl of-}
            {--- simply drop the task.-}
            {-CannotBeCompleted s -> (AbortedTask task s : err,lvl, tm)-}

            {--- drop the current state and readd the task as inactive.-}
            {-Reschedule          -> (err,lvl, addTask task tm)-}

            {--- update the task state in the manager and the modified level.-}
            {-InProgress lvl'     -> (err,lvl', tm & active %~ insert (ActiveTask task state'))-}

    insert = (:)
