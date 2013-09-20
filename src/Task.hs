{-# LANGUAGE TemplateHaskell #-}
module Task ( Task (..)
            , taskId
            , taskTarget
            , taskType
            , TaskType (..)
            ) where

import Control.Lens.TH
import Types
import Coords

data TaskType = Mine | Lumber

data Task = Task { _taskId :: Identifier
                 , _taskTarget :: Coord
                 , _taskType :: TaskType
                 }
makeLenses ''Task
