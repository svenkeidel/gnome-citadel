module Level.Task where

import Control.Lens.Operators

import Level
import qualified Level.Command as Command
import StaticElement(StaticElement)
import Task

mine :: StaticElement -> Level -> Identifier Task -> Task
mine s lvl i = Task i (lvl ^. coordOf s) Mine (Command.mine s)
