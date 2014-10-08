module Level.Task where

import           Control.Lens.Operators

import           Data.Monoid ((<>))

import           Actor (TaskType(Mine))
import           Level
import qualified Level.Command as Command
import           StaticElement (StaticElement, Category(..))
import           Task
import           Counter

mine :: StaticElement -> Level -> Identifier Task -> Task
mine s lvl i = Task
             { _id = i
             , _target = lvl ^. coordOfTile s
             , _taskType = Mine
             , _command = \dwarf -> Command.mine s dwarf `catch` handler
             , _precondition = \lvl' actor ->
                 unless (isReachable (lvl' ^. coordOfTile s) lvl')
                   "Mining target is unreachable"
                 <>
                 unless (holdsSuitableTool lvl' actor Mining)
                   "Actor holds no suitable tool for Mining"
             }
  where
    handler _ = Reschedule
