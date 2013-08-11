{-# LANGUAGE TemplateHaskell #-}
module Task ( Task (..)
            , taskId
            , taskTarget
            , taskType
            , TaskType (..)
            , TaskQueue
            ) where

import qualified Data.Sequence as S
import Control.Lens.TH
import Types

data TaskType = Mine | Lumber

data Task = Task { _taskId :: Identifier
                 , _taskTarget :: Identifier
                 , _taskType :: TaskType
                 }
makeLenses ''Task

type TaskQueue = S.Seq Task
