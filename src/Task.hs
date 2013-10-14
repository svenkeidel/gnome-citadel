{-# LANGUAGE TemplateHaskell #-}
module Task ( Task (..)
            , id
            , target
            , taskType
            , command
            ) where

import Prelude hiding (id)
import Control.Lens((^.))
import Control.Lens.TH

import Data.Ord(comparing)

import Types
import Coords
import Level.Command(Command)
import Actor(Actor)

data Task = Task { _id :: Identifier Task
                 , _target :: Coord
                 , _taskType :: TaskType
                 , _command :: Actor -> Command
                 }
makeLenses ''Task

instance Show Task where
  show task = "Task "
           ++ "{ _id = " ++ (show $ task ^. id)
           ++ ", _target = " ++ (show $ task ^. target)
           ++ ", _type = " ++ (show $ task ^. taskType)
           ++ " }"

instance Eq Task where
  t1 == t2 = t1 ^. id == t2 ^. id

instance Ord Task where
  compare = comparing (^. id)
