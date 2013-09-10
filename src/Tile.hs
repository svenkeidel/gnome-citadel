{-# LANGUAGE TemplateHaskell, EmptyDataDecls #-}
module Tile where

import Types
import StaticElement
import Actor
import Control.Applicative
import Control.Lens.Getter
import Control.Lens.TH

data Tile = Tile
          { _tileId :: Identifier
          , _tileCharRepr :: Char
          } deriving Show
makeLenses ''Tile

class TileRepr t where
  toTile :: t -> Tile

instance TileRepr StaticElement where
  toTile = Tile <$> view staticElementId <*> view staticElementCharRepr

instance TileRepr Actor where
  toTile = Tile <$> view actorId <*> view actorCharRepr

instance TileRepr Tile where
  toTile = id

wall :: StaticElement
wall = StaticElement undefined '#'

free :: StaticElement
free = StaticElement undefined ' '

dwarf :: Actor
dwarf = Actor undefined '@'
