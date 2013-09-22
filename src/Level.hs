{-# LANGUAGE TemplateHaskell, RankNTypes, FlexibleContexts, ScopedTypeVariables #-}
module Level ( Level (..)
             , emptyLevel

             {- Lenses -}
             , actors
             , staticElements
             , nextFreeId
             , bounds
             , idToCoord
             , coordToId
             , walkable
             , taskManager

             , fromString
             , at
             , freshId
             , coordOf
             , findPath
             , findArea
             , findActor
             , findStaticElement
             , isWalkable
             , isReachable
             , deleteFromCoords

             , module Coords
             , module Types
             ) where


import Control.Lens ((^.),(%=),(<+=),(%%=),(.~),(.=),ix,Lens',lens)
import Control.Lens.TH
import Control.Monad.State
import Control.Applicative
import Data.Default

import qualified Control.Lens.Getter as LG
import qualified Data.Map as M
import qualified Data.List as L
import qualified TaskManagement as T

import Data.Maybe(mapMaybe,fromMaybe)

import Types
import Tile
import Renderable
import Coords
import Utils

import Actor(Actor)
import StaticElement(StaticElement)
import qualified Actor
import qualified StaticElement
import qualified Path as P

data Level = Level { _actors            :: M.Map Identifier Actor
                   , _staticElements    :: M.Map Identifier StaticElement
                   , _nextFreeId        :: Identifier
                   , _bounds            :: (Int, Int)
                   , _idToCoord         :: M.Map Identifier Coord
                   , _coordToId         :: M.Map Coord [Identifier]
                   , _walkable          :: Level -> Coord -> Bool
                   , _taskManager       :: T.TaskManager
                   }
makeLenses ''Level

instance Show Level where
  show = (^.toString)

-- | smart constructor for an empty level
emptyLevel :: Level
emptyLevel = Level { _actors = def
                   , _staticElements = def
                   , _nextFreeId = def
                   , _bounds = def
                   , _idToCoord = def
                   , _coordToId = def
                   , _walkable = error "walkable heuristik undefined"
                   , _taskManager = def
                   }

-- | returns a fresh identifier that is used to refernce tiles inside
-- the level.
freshId :: MonadState Level m => m Int
freshId = nextFreeId <+= 1

type TileBuilder = Char -> Maybe (Either Actor StaticElement)

-- | turns the given string into a level. It uses a builder function
-- that returns actors or static elements based on the characters that
-- the string contains.
fromString :: TileBuilder -> String -> Level
fromString builder str = execState (mapM insert coordStr) emptyLevel
  where
    coordStr            = concat $ (zipWith . zipWith) (,) coords (lines str)
    coords              = [ [ from2d (x,y) | x <- [0..] ] | y <- [0..] ] :: [[Coord]]
    maxT (Coord x1 y1 _) (x2,y2) = (max x1 x2, max y1 y2)
    insert (coord,char) = do
      nextId <- freshId
      case builder char of
        Just (Left  a) -> actors         %= M.insert nextId (Actor.id .~ nextId $ a)
        Just (Right s) -> staticElements %= M.insert nextId (StaticElement.id .~ nextId $ s)
        Nothing        -> return ()
      idToCoord %= M.insert nextId coord
      coordToId %= M.insertWith (++) coord [nextId]
      bounds    %= maxT coord

-- | turns a level into a string. It pads the regions that contain no
-- tiles with spaces until the maximum coordinates are reached.
toString :: LG.Getter Level String
toString = LG.to getter
  where
    getter lvl = unlines $ (map . map) (\c -> render $ lvl ^. at c) coords
      where
        (mx,my) = lvl ^. bounds
        coords  = [ [ from2d (x,y) | x <- [0..mx] ] | y <- [0..my] ] :: [[Coord]]

-- | returns a list of tiles located at the given coordinate inside the level
at :: Coord -> LG.Getter Level [Tile]
at coord = LG.to getter
  where
    getter lvl = mapMaybe lookupTile ids
      where
        ids = M.findWithDefault [] coord (lvl ^. coordToId)
        lookupTile ident =  toTile <$> M.lookup ident (lvl ^. actors)
                        <|> toTile <$> M.lookup ident (lvl ^. staticElements)

-- | gets and manipulates the coordinate at whitch the given tile is located
--
-- @
-- lvl ^. coordOf dwarf
-- @
--
-- @
-- lvl & coordOf dwarf .~ coord
-- @
coordOf :: TileRepr t => t -> Lens' Level Coord
coordOf tile = lens getter setter
  where
    getter lvl =
      fromMaybe (error $ "the identifer '" ++ show (toTile tile) ++ "' has no assigned coordinate")
      $ M.lookup (toTile tile ^. Tile.id) (lvl ^. idToCoord)
    setter lvl dst = flip execState lvl $ do
      idToCoord . ix tid .= dst
      coordToId . ix src %= L.delete tid
      coordToId . ix dst %= (tid :)
      where
        tid = toTile tile ^. Tile.id
        src = getter lvl


isWalkable :: Coord -> LG.Getter Level Bool
isWalkable c = LG.to (\lvl -> _walkable lvl lvl c)

-- | searches a path from one coordinate in a level to another. It
-- uses the walkable heuristik to find a suitable path.
findPath :: Coord -> Coord -> LG.Getter Level (Maybe P.Path)
findPath from to = LG.to (\lvl -> P.defaultPath (lvl ^-> walkable) from to)

findArea :: Coord -> [Coord] -> LG.Getter Level (Maybe P.Path)
findArea from to = LG.to (\lvl -> P.findArea (lvl ^-> walkable) from to)

-- | filters tiles of a level by a predicate and returns the
-- satisfying tile as a list.
findTile :: TileRepr t => LG.Getter Level (M.Map Identifier t) -> (t -> Bool) -> LG.Getter Level [t]
findTile tileGetter f = LG.to (\lvl -> M.elems $ M.filter f $ lvl ^. tileGetter)

findActor :: (Actor -> Bool) -> LG.Getter Level [Actor]
findActor = findTile actors

findStaticElement :: (StaticElement -> Bool) -> LG.Getter Level [StaticElement]
findStaticElement = findTile staticElements

deleteFromCoords :: MonadState Level m => TileRepr t => t -> m ()
deleteFromCoords t = do
  c <- idToCoord %%= deleteLookup tid
  maybe (return ()) (\c' -> coordToId . ix c' %= L.delete tid) c
  where
    tid = toTile t ^. Tile.id
    deleteLookup = M.updateLookupWithKey (const . const Nothing)

isReachable :: Coord -> Level -> Bool
isReachable = const $ const False
