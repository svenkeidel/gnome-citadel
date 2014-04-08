module Level.Task where

import Control.Lens.Operators

import Level
import StaticElement(StaticElement)
import Task
import Counter
import Actor (TaskType(Mine))
import Control.Exception(assert)

mine :: StaticElement -> Level -> Identifier Task -> Task
mine s lvl i = Task
             { _id = i
             , _target = lvl ^. coordOf s
             , _taskType = Mine
             , _command = assert False undefined --Command.mine s
             , _precondition = \lvl' -> isReachable (lvl' ^. coordOf s) lvl'
             }
