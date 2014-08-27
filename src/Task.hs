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
            ) where

import Prelude hiding (id)
import Control.Lens((^.))
import Control.Lens.TH

import Data.Ord(comparing)
import Data.Monoid

import Counter
import Coords
import Actor(Actor, TaskType)
import Unfold

import Level

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
                 , _command :: Actor -> Level -> Unfold (Level -> TaskStatus)
                 , _precondition :: Level -> Actor -> Condition String
                 }
makeLenses ''Task

instance Show Task where
  show task = "Task "
           ++ "{ _id = " ++ show (task ^. id)
           ++ ", _target = " ++ show (task ^. target)
           ++ ", _type = " ++ show (task ^. taskType)
           ++ " }"

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
data ActiveTask = ActiveTask Task (Unfold (Level -> TaskStatus))

instance Eq ActiveTask where
  (ActiveTask t1 _) == (ActiveTask t2 _) = t1 == t2
