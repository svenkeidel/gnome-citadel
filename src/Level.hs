{-# LANGUAGE TemplateHaskell #-}
module Level ( Level (..)
             , actors
             , staticElements
             , nextFreeId
             , activeTaskQueue
             , inactiveTaskQueue
             , fromString
             , at
             , createTask
             , numberOfTasks
             ) where

import Control.Lens.TH
import Actor
import StaticElement
import Types
import Tile
import Task

data Level = Level { _actors :: [Actor]
                   , _staticElements :: [StaticElement]
                   , _nextFreeId :: Identifier
                   , _activeTaskQueue :: TaskQueue
                   , _inactiveTaskQueue :: TaskQueue
                   }
makeLenses ''Level

createTask :: TaskType -> Coord -> Level -> Level
createTask = undefined

numberOfTasks :: Level -> Int
numberOfTasks = undefined

fromString :: String -> Level
fromString = undefined

at :: Level -> Coord -> Tile
at = undefined
