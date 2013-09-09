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
             , walkable

             , fromString
             , at
             , freshId
             , findPath
             , getCoord
             , findActor

             , (^->)

             , module Coords
             , module Types
             ) where


import Control.Lens ((^.),(%=),(<+=),(.~))
import Control.Lens.TH
import Control.Monad.State
import Control.Applicative
import Control.Monad.Reader

import qualified Control.Lens.Getter as LG
import qualified Data.Sequence as S
import qualified Data.HashMap.Lazy as H

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

data Level = Level { _actors            :: H.HashMap Identifier Actor
                   , _staticElements    :: H.HashMap Identifier StaticElement
                   , _nextFreeId        :: Identifier
                   , _activeTaskQueue   :: Queue Task
                   , _inactiveTaskQueue :: Queue Task
                   , _bounds            :: (Int, Int)
                   , _idToCoord         :: H.HashMap Identifier Coord
                   , _coordToId         :: H.HashMap Coord [Identifier]
                   , _walkable          :: Level -> Coord -> Bool
                   }
makeLenses ''Level

instance Show Level where
  show = toString

-- | smart constructor for an empty level
emptyLevel :: Level
emptyLevel =
  Level
  { _actors = H.empty
  , _staticElements = H.empty
  , _nextFreeId = 0
  , _activeTaskQueue = S.empty
  , _inactiveTaskQueue = S.empty
  , _bounds = (0, 0)
  , _idToCoord = H.empty
  , _coordToId = H.empty
  , _walkable = error "walkable heuristik undefined"
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
        Just (Left  a) -> actors         %= H.insert nextId (actorId .~ nextId $ a)
        Just (Right s) -> staticElements %= H.insert nextId (staticElementId .~ nextId $ s)
        Nothing        -> return ()
      idToCoord %= H.insert nextId coord
      coordToId %= H.insertWith (++) coord [nextId]
      bounds    %= maxT coord

-- | turns a level into a string. It pads the regions that contain no
-- tiles with spaces until the maximum coordinates are reached.
toString :: Level -> String
toString lvl = unlines $ (map . map) (render . at lvl) coords
  where
    (mx,my) = lvl ^. bounds
    coords  = [ [ from2d (x,y) | x <- [0..mx] ] | y <- [0..my] ] :: [[Coord]]

-- | returns a list of tiles located at the given coordinate inside the level
at :: Level -> Coord -> [Tile]
at lvl coord = mapMaybe lookupTile ids
  where
    ids = fromMaybe [] $ H.lookup coord (lvl ^. coordToId)
    lookupTile ident =  toTile <$> H.lookup ident (lvl ^. actors)
                    <|> toTile <$> H.lookup ident (lvl ^. staticElements)

-- | gets the coordinate at whitch the given tile is located
getCoord :: (TileRepr t, Functor m, MonadReader Level m) => t -> m Coord
getCoord tile = fromMaybe (error $ "the identifer '" ++ show (toTile tile) ++ "' has no assigned coordinate")
              . H.lookup idT <$> LG.view idToCoord
  where
    idT = toTile tile ^. tileId

-- | dereferences a 'method' of a type
-- useful when a field of a record takes the record itself as a first parameter:
--
-- @
-- data Level =
--   Level {
--     walkable :: Level -> Coord -> Bool
--     ...
--   }
-- @
--
-- >>> lvl ^-> walkable $ (1,3)
(^->) :: s -> LG.Getting (s -> a) s (s -> a) -> a
s ^-> a = (s ^. a) s

-- | searches a path from one coordinate in a level to another. It
-- uses the walkable heuristik to find a suitable path.
findPath :: MonadReader Level m => Coord -> Coord -> m (Maybe Path)
findPath from to = do
  level <- ask
  return $ defaultPath (level ^-> walkable) from to

-- | filters actors of a level by a predicate and returns the
-- satisfying actors as a list.
findActor :: (Functor m, MonadReader Level m) => (Actor -> Bool) -> m [Actor]
findActor f = (H.elems . H.filter f) <$> LG.view actors
