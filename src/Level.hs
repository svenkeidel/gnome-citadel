{-# LANGUAGE TemplateHaskell, RankNTypes, FlexibleContexts, ScopedTypeVariables #-}
module Level ( Level (..)
             , emptyLevel

             , actors
             , staticElements
             , bounds
             , idToCoord
             , coordToId
             , walkable

             , fromString
             , at
             , actorsAt
             , staticElementsAt
             , coordOf
             , findPath
             , findArea
             , findActor
             , findStaticElement
             , isWalkable
             , inBounds
             , isReachable
             , deleteFromCoords
             , actorInventory
             , findTool

             , addActor
             , addItem

             , TileBuilder

             , module Coords
             ) where

import           Control.Lens (ix, lens, Lens', to, view, Fold, lastOf)
import           Control.Lens.At (contains)
import           Control.Lens.Fold (folded, elemOf, findOf)
import           Control.Lens.Operators
import           Control.Lens.TH
import           Data.Default

import qualified Control.Lens.Getter as LG
import qualified Data.Map as M
import qualified Data.List as L

import           Data.Maybe (mapMaybe,fromMaybe,isJust)

import           Counter
import           Tile
import           Renderable
import           Coords
import           Utils

import           Actor (Actor)
import           StaticElement (StaticElement,Category)
import qualified Actor
import qualified StaticElement
import qualified Path as P

data Level = Level { _actors            :: M.Map (Identifier Actor) Actor
                   , _staticElements    :: M.Map (Identifier StaticElement) StaticElement
                   , _bounds            :: (Int, Int)
                   , _idToCoord         :: M.Map (Identifier (Either Actor StaticElement)) Coord
                   , _coordToId         :: M.Map Coord [Identifier (Either Actor StaticElement)]
                   , _walkable          :: Level -> Coord -> Bool
                   }
makeLenses ''Level

instance Show Level where
  show = toString

-- | smart constructor for an empty level
emptyLevel :: Level
emptyLevel = Level { _actors = def
                   , _staticElements = def
                   , _bounds = def
                   , _idToCoord = def
                   , _coordToId = def
                   , _walkable = error "walkable heuristik undefined"
                   }

type TileBuilder = Identifier Tile -> Char -> Maybe (Either Actor StaticElement)

-- | turns the given string into a level. It uses a builder function
-- that returns actors or static elements based on the characters that
-- the string contains.
fromString :: TileBuilder -> String -> Counter -> (Level,Counter)
fromString builder str cnt0 = foldr insert (emptyLevel,cnt0) coordStr
  where
    coordStr            = concat $ (zipWith . zipWith) (,) coords (lines str)
    coords              = [ [ from2d (x,y) | x <- [0..] ] | y <- [0..] ] :: [[Coord]]
    maxT (Coord x1 y1 _) (x2,y2) = (max x1 x2, max y1 y2)
    insert (coord,char) (lvl,cnt) =
      let (nextId,cnt') = freshId cnt
          lvl' = lvl & case builder nextId char of
                        Just (Left  a) -> addActor (Actor.id .~ nextId $ a)
                        Just (Right s) -> addItem (StaticElement.id .~ nextId $ s)
                        Nothing        -> Prelude.id
                    & idToCoord %~ M.insert nextId coord
                    & coordToId %~ M.insertWith (++) coord [nextId]
                    & bounds    %~ maxT coord
      in (lvl',cnt')

-- | turns a level into a string. It pads the regions that contain no
-- tiles with spaces until the maximum coordinates are reached.
toString :: Level -> String
toString lvl = unlines $ (map . map) (\c -> render . at c $ lvl) coords
  where
    (mx,my) = lvl ^. bounds
    coords  = [ [ from2d (x,y) | x <- [0..mx] ] | y <- [0..my] ] :: [[Coord]]

-- | returns a list of tiles located at the given coordinate inside the level
at :: Coord -> Level -> [Tile]
at coord lvl = map toTile (actorsAt coord lvl) ++ map toTile (staticElementsAt coord lvl)

actorsAt :: Coord -> Level -> [Actor]
actorsAt coord lvl = mapMaybe lookupTile ids
  where
    ids = M.findWithDefault [] coord (lvl ^. coordToId)
    lookupTile ident = M.lookup (asIdentifierOf ident) (lvl ^. actors)

staticElementsAt :: Coord -> Level -> [StaticElement]
staticElementsAt coord lvl = mapMaybe lookupTile ids
  where
    ids = M.findWithDefault [] coord (lvl ^. coordToId)
    lookupTile ident = M.lookup (asIdentifierOf ident) (lvl ^. staticElements)

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
      $ M.lookup (toTile tile ^. Tile.id . to asIdentifierOf) (lvl ^. idToCoord)
    setter lvl dst = lvl
                   & idToCoord . ix tid .~ dst
                   & coordToId . ix src %~ L.delete tid
                   & coordToId . ix dst %~ (tid :)
      where
        tid = asIdentifierOf $ toTile tile ^. Tile.id
        src = getter lvl

isWalkable :: Coord -> Level -> Bool
isWalkable c lvl = _walkable lvl lvl c

inBounds :: Coord -> Level -> Bool
inBounds (Coord x y _) lvl = x `elem` [0..mx] && y `elem` [0..my]
  where (mx,my) = view bounds lvl

-- | searches a path from one coordinate in a level to another. It
-- uses the walkable heuristik to find a suitable path.
findPath :: Coord -> Coord -> Level -> Maybe P.Path
findPath from dest lvl = P.defaultPath (lvl ^-> walkable) from dest

findArea :: Coord -> [Coord] -> Level -> Maybe P.Path
findArea from dest lvl = P.findArea (lvl ^-> walkable) from dest

-- | filters tiles of a level by a predicate and returns the
-- satisfying tile as a list.
findTile :: TileRepr t =>  (Level -> M.Map (Identifier a) t) -> (t -> Bool) -> Level -> [t]
findTile tileGetter f = M.elems . M.filter f . tileGetter

findActor :: (Actor -> Bool) -> Level -> [Actor]
findActor = findTile (view actors)

findStaticElement :: (StaticElement -> Bool) -> Level -> [StaticElement]
findStaticElement = findTile (view staticElements)

deleteFromCoords :: TileRepr t => t -> Level -> Level
deleteFromCoords t level = maybe level' (\c' -> level' & coordToId . ix c' %~ L.delete tid) c
  where
    (c, level') = level & idToCoord %%~ deleteLookup tid
    tid = asIdentifierOf $ toTile t ^. Tile.id
    deleteLookup = M.updateLookupWithKey (const . const Nothing)

isReachable :: Coord -> Level -> Bool
isReachable target lvl =
  any canReach (lvl ^. actors . LG.to M.elems)
  where canReach :: Actor -> Bool
        canReach actor = isJust $ do
          actorCoord <- M.lookup (asIdentifierOf $ actor ^. Actor.id) (lvl ^. idToCoord)
          P.defaultPath (lvl ^-> walkable) actorCoord target

actorInventory :: Level -> Actor -> [StaticElement]
actorInventory lvl actor = map ((lvl ^. staticElements) M.!) invIds
  where invIds = actor ^. Actor.inventory

findTool :: Category -> Coord -> Level -> Maybe StaticElement
findTool cat coord lvl = do
  p <- P.searchPath (lvl ^-> walkable) (const 1) (const . const 1) coord predicate
  c <- lastOf (P.pathCoords . folded) p
  findOf folded (^. StaticElement.category . contains cat) $ staticElementsAt c lvl
  where predicate c = elemOf categories cat $ staticElementsAt c lvl
        categories :: Fold [StaticElement] Category
        categories = folded . StaticElement.category . folded

addItem :: StaticElement -> Level -> Level
addItem item = staticElements %~ M.insert (item ^. StaticElement.id) item

addActor :: Actor -> Level -> Level
addActor actor = actors %~ M.insert (actor ^. Actor.id) actor
