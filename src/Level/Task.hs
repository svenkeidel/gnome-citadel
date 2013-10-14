module Level.Task where

import Level
import Level.Command
import StaticElement(StaticElement)
import Task

mine :: StaticElement -> Level -> Identifier Task -> Task
mine s lvl i = Task i (lvl ^. coordOf s) Mine (Command.mine s)
