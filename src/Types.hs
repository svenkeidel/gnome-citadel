{-# LANGUAGE TemplateHaskell #-}
module Types ( Task (..)
             , taskId
             , taskTarget
             , taskType

             , Actor (..)
             , actorId

             , StaticElement (..)
             , staticElementId

             , Level (..)
             , actors
             , staticElements
             , nextFreeId
             , activeTaskQueue
             , inactiveTaskQueue

             , TaskType (..)
             ) where


import qualified Data.Sequence as S
import Control.Lens.TH

type Identifier = Int

type TaskQueue = S.Seq Task

data TaskType = Mine | Lumber

data Task = Task { _taskId :: Identifier
                 , _taskTarget :: Identifier
                 , _taskType :: TaskType
                 }
makeLenses ''Task

data Actor = Actor { _actorId :: Identifier
                   {- ... -}
                   }
makeLenses ''Actor

data StaticElement = StaticElement { _staticElementId :: Identifier
                                   {- ... -}
                                   }

makeLenses ''StaticElement

data Level = Level { _actors :: [Actor]
                   , _staticElements :: [StaticElement]
                   , _nextFreeId :: Identifier
                   , _activeTaskQueue :: TaskQueue
                   , _inactiveTaskQueue :: TaskQueue
                   }
makeLenses ''Level
