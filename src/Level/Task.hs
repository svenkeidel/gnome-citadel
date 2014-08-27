module Level.Task where

import           Control.Lens.Operators
import           Data.Monoid (mconcat)

import           Actor (TaskType(Mine))
import           Level
import qualified Level.Command as Command
import           StaticElement (StaticElement, Category(..))
import           Task
import           Counter
import           Unfold
import           Level.Transformation

mine :: StaticElement -> Level -> Identifier Task -> Task
mine s lvl i = Task
             { _id = i
             , _target = lvl ^. coordOf s
             , _taskType = Mine
             , _command = \dwarf lvl' -> Command.mine s dwarf lvl' `catch` handler
             , _precondition = \lvl' actor ->
                 mconcat [ unless (isReachable (lvl' ^. coordOf s) lvl')
                            "Mining target is unreachable"
                         , unless (holdsSuitableTool lvl' actor Mining)
                           "Actor holds no suitable tool for Mining"
                         ]
             }
  where
    catch :: Unfold LevelTrans -> (LevelError -> TaskStatus) -> Unfold (Level -> TaskStatus)
    catch cmd h = fmap (\cmd' lvl' -> either h InProgress (cmd' lvl')) cmd

    handler _ = Reschedule
