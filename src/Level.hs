{-# LANGUAGE TemplateHaskell, RankNTypes, FlexibleContexts #-}
module Level ( Level (..)
             , emptyLevel

             {- Lenses -}
             , actors
             , staticElements
             , nextFreeId
             , activeTaskQueue
             , inactiveTaskQueue
             , bounds
             , idToCoord
             , coordToId

             , fromString
             , at
             , createTask
             , numberOfTasks
             , hasTask
             , getTask
             , freshId
             , findPath
             , getCoord
             , walkable

             , module Coords
             , module Types
             ) where

import Control.Lens ((^.),(%=),(<+=))
import Control.Lens.TH
import Control.Monad.State
import Control.Applicative
import Control.Monad.Reader.Class

import qualified Data.Sequence as S
import qualified Data.Foldable as F
import qualified Control.Lens.Getter as LG
import qualified Data.Map as M
import qualified Data.Monoid as DM

import Data.Maybe(mapMaybe,fromMaybe)

import Path
import Actor
import StaticElement
import Types
import Tile
import Task
import Queue
import Renderable
import Coords


data Level = Level { _actors            :: M.Map Identifier Actor
                   , _staticElements    :: M.Map Identifier StaticElement
                   , _nextFreeId        :: Identifier
                   , _activeTaskQueue   :: Queue Task
                   , _inactiveTaskQueue :: Queue Task
                   , _bounds            :: (Int, Int)
                   , _idToCoord         :: M.Map Identifier Coord
                   , _coordToId         :: M.Map Coord [Identifier]
                   }
makeLenses ''Level

instance Show Level where
  show = toString

-- | smart constructor for an empty level
emptyLevel :: Level
emptyLevel =
  Level
  { _actors = M.empty
  , _staticElements = M.empty
  , _nextFreeId = 0
  , _activeTaskQueue = S.empty
  , _inactiveTaskQueue = S.empty
  , _bounds = (0, 0)
  , _idToCoord = M.empty
  , _coordToId = M.empty
  }

createTask :: MonadState Level m => Coord -> TaskType -> m Task
createTask coord tType = do
  currentLevel <- get
  nextId <- freshId
  let task = Task nextId coord tType
      targetQueue = if isReachable coord currentLevel
                       then activeTaskQueue
                       else inactiveTaskQueue
  targetQueue %= enqueue task
  idToCoord %= M.insert nextId coord
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

freshId :: MonadState Level m => m Int
freshId = nextFreeId <+= 1

type TileBuilder = Char -> Either Actor StaticElement

fromString :: TileBuilder -> String -> Level
fromString builder str = execState (mapM insert coordStr) emptyLevel
  where
    coordStr               = concat $ (zipWith . zipWith) (,) coords (lines str)
    coords                 = [ [ from2d (x,y) | x <- [0..] ] | y <- [0..] ] :: [[Coord]]
    maxT (Coord x1 y1 _) (x2,y2) = (max x1 x2, max y1 y2)
    insert (coord,char)
      | char == ' ' = return ()
      | otherwise   = do
          nextId <- freshId
          case builder char of
            Left  a -> actors         %= M.insert nextId a
            Right s -> staticElements %= M.insert nextId s
          idToCoord %= M.insert nextId coord
          coordToId %= M.insertWith (++) coord [nextId]
          bounds    %= maxT coord

toString :: Level -> String
toString lvl = unlines $ (map . map) (render . at lvl) coords
  where
    (mx,my) = lvl ^. bounds
    coords  = [ [ from2d (x,y) | x <- [0..mx] ] | y <- [0..my] ] :: [[Coord]]

at :: Level -> Coord -> [Tile]
at lvl coord = mapMaybe lookupTile ids
  where
    ids = M.findWithDefault [] coord (lvl ^. coordToId)
    lookupTile ident =  toTile <$> M.lookup ident (lvl ^. actors)
                    <|> toTile <$> M.lookup ident (lvl ^. staticElements)

getCoord :: (Functor m, MonadReader Level m) => Identifier -> m (Coord)
getCoord ident = fromMaybe (error $ "the identifer '" ++ show ident ++ "' has no assigned coordinate")
               . M.lookup ident <$> LG.view idToCoord

walkable :: Level -> Coord -> Bool
walkable = undefined

findPath :: (MonadReader Level m) => Coord -> Coord -> m (Maybe Path)
findPath from to = do
  level <- ask
  return $ defaultPath (walkable level) from to
