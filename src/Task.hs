{-# LANGUAGE TemplateHaskell #-}
module Task ( Task (..)
            , taskId
            , taskTarget
            , taskType
            , TaskType (..)
            , mine
            ) where

import Control.Lens.TH
import Types
import Coords

data TaskType = Mine | Lumber deriving (Show,Eq)

data Task = Task { _taskId :: Identifier
                 , _taskTarget :: Coord
                 , _taskType :: TaskType
                 } deriving (Show,Eq)
makeLenses ''Task

mine :: Coord -> Identifier -> Task
mine c i = Task i c Mine

