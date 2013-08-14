{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module Level ( Level (..)
             {- Lenses -}
             , actors
             , staticElements
             , nextFreeId
             , activeTaskQueue
             , inactiveTaskQueue
             , idToCoord

             , fromString
             , at
             , createTask
             , numberOfTasks
             , hasTask
             , getTask
             ) where

import Control.Lens ((^.),(%=),(+=))
import Control.Lens.TH
import Control.Monad.State
import Control.Applicative

import qualified Data.Sequence as S
import qualified Data.Foldable as F
import qualified Control.Lens.Getter as LG
import qualified Data.Map as M
import qualified Data.Monoid as DM

import Actor
import StaticElement
import Types
import Tile
import Task
import Queue

data Level = Level { _actors :: [Actor]
                   , _staticElements :: [StaticElement]
                   , _nextFreeId :: Identifier
                   , _activeTaskQueue :: Queue Task
                   , _inactiveTaskQueue :: Queue Task
                   , _idToCoord :: M.Map Identifier Coord
                   }
makeLenses ''Level

createTask :: Coord -> TaskType -> State Level Task
createTask coord tType = do
  currentLevel <- get
  let nextId = currentLevel ^. nextFreeId
      task = Task nextId coord tType
      targetQueue = if isReachable coord currentLevel
                       then activeTaskQueue
                       else inactiveTaskQueue
  targetQueue %= enqueue task
  idToCoord %= M.insert nextId coord
  nextFreeId += 1
  return task

isReachable :: Coord -> Level -> Bool
isReachable = const $ const False

hasTask :: Identifier -> Level -> Bool
hasTask tId lvl = F.any match (lvl ^. activeTaskQueue) ||
                  F.any match (lvl ^. inactiveTaskQueue)
  where
    match t = t ^. taskId == tId

getTask :: Identifier -> Level -> Maybe (Coord,Task)
getTask tId lvl = (,) <$> taskCoordinate <*> foundTask
  where
    taskCoordinate :: Maybe Coord
    taskCoordinate = M.lookup tId (lvl ^. idToCoord)

    foundTask :: Maybe Task
    foundTask = useFirst [ findTaskInQueue activeTaskQueue
                         , findTaskInQueue inactiveTaskQueue
                         ]

    findTaskInQueue :: LG.Getter Level (Queue Task) -> Maybe Task
    findTaskInQueue queue = findTask lvl queue (matchId tId)

matchId :: Identifier -> Task -> Bool
matchId tId task = tId == task ^. taskId

findTask :: Level -> LG.Getter Level (Queue Task) -> (Task -> Bool) -> Maybe Task
findTask lvl queue p = F.find p $ lvl ^. queue

useFirst:: F.Foldable t => t (Maybe a) -> Maybe a
useFirst = DM.getFirst . F.foldMap DM.First

numberOfTasks :: Level -> Int
numberOfTasks lvl = numberOfActiveTasks lvl + numberOfInactiveTasks lvl

numberOfActiveTasks :: Level -> Int
numberOfActiveTasks lvl = S.length $ lvl ^. activeTaskQueue

numberOfInactiveTasks :: Level -> Int
numberOfInactiveTasks lvl = S.length $ lvl ^. inactiveTaskQueue

fromString :: String -> Level
fromString = undefined

at :: Level -> Coord -> Tile
at = undefined