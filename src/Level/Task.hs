module Level.Task where

import           Control.Lens.Operators

import           Data.Monoid ((<>))

import           Actor (TaskType(Mine))
import           Level
import qualified Level.Command as Command
import           StaticElement (StaticElement, Category(..))
import           Task
import           Counter

mine :: StaticElement -> Level -> Identifier Task -> Maybe Task
mine s lvl i = do
  t <- lvl ^? coordOfTile s
  return Task { _id = i
              , _target = t
              , _taskType = Mine
              , _command = \dwarf -> Command.mine s dwarf `catch` handler
              , _precondition = \lvl' actor ->
                  unless (maybe False (`isReachable` lvl') (lvl' ^? coordOfTile s))
                    "Mining target is unreachable"
                  <>
                  unless (holdsSuitableTool lvl' actor Mining)
                    "Actor holds no suitable tool for Mining"
              }
  where
    handler _ = Reschedule
