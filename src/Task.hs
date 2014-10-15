{-# LANGUAGE TemplateHaskell #-}
module Task ( Task (..)
            , TaskStatus(..)
            , ActiveTask(..)
            , id
            , target
            , taskType
            , command
            , precondition
            , unless
            , catch
            ) where

import Prelude hiding (id)

import Control.Coroutine
import Control.Coroutine.AwaitYield

import Control.Lens((^.))
import Control.Lens.TH
import Control.DeepSeq (NFData)
import Control.DeepSeq (rnf)

import Data.Ord(comparing)
import Data.Monoid
import Data.Functor.Identity

import Counter
import Coords
import Actor(Actor, TaskType)

import Level
import Level.Transformation

data TaskStatus
  = CannotBeCompleted String
  | Reschedule
  | InProgress Level

data Condition w = Failed w | Satisfied deriving (Show,Eq)

instance Monoid w => Monoid (Condition w) where
  mempty = Satisfied
  Failed w  `mappend` Failed u  = Failed (w `mappend` u)
  Failed w  `mappend` Satisfied = Failed w
  Satisfied `mappend` x         = x

unless :: Bool -> a -> Condition a
unless b s | b         = Satisfied
         | otherwise = Failed s

data Task = Task { _id :: Identifier Task
                 , _target :: Coord
                 , _taskType :: TaskType
                 , _command :: Actor -> Coroutine (AwaitYield Level TaskStatus) Identity ()
                 , _precondition :: Level -> Actor -> Condition String
                 }
makeLenses ''Task

catch :: Coroutine (AwaitYield Level Level) (Either LevelError) ()
      -> (LevelError -> TaskStatus)
      -> Coroutine (AwaitYield Level TaskStatus) Identity ()
catch (Coroutine c) f = 
  case c of
    Left l  -> do
      yield (f l)
    Right (Right ()) -> return ()
    Right (Left (Await g)) -> do
      lvl <- await
      g lvl `catch` f
    Right (Left (Yield lvl c')) -> do
      yield (InProgress lvl)
      c' `catch` f

instance NFData Task where
  rnf (Task (Identifier tid) tgt typ _ _) = rnf (tid,tgt,typ)


instance Show Task where
  show task = "Task "
           ++ "{ id = " ++ show (task ^. id)
           ++ ", target = " ++ show (task ^. target)
           ++ ", type = " ++ show (task ^. taskType)
           ++ " }"

instance Symbol Task where
  sym _ = "Task"

instance Eq Task where
  t1 == t2 = t1 ^. id == t2 ^. id

instance Ord Task where
  compare = comparing (^. id)

instance Show ActiveTask where
  show (ActiveTask task _)
    = "ActiveTask ("
   ++ show task
   ++ ")"

-- Stores the original task and the current state of the execution of the task.
data ActiveTask = ActiveTask Task (Coroutine (AwaitYield Level TaskStatus) Identity ())

instance NFData ActiveTask where
  rnf (ActiveTask x1 _) = rnf x1

instance Eq ActiveTask where
  (ActiveTask t1 _) == (ActiveTask t2 _) = t1 == t2
