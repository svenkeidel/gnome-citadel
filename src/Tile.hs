{-# LANGUAGE TemplateHaskell, EmptyDataDecls #-}
module Tile ( Tile(..)
            , Tile.id
            , charRepr
            , TileRepr(..)
            ) where

import Counter
import Control.Lens.Getter
import Control.Lens.TH

import Actor(Actor)
import StaticElement(StaticElement)
import qualified Actor
import qualified StaticElement

data Tile = Tile
          { _id :: Identifier Tile
          , _charRepr :: Char
          } deriving Show
makeLenses ''Tile

class TileRepr t where
  toTile :: t -> Tile

instance TileRepr StaticElement where
  toTile s = Tile (asIdentifierOf $ s ^. StaticElement.id) (s ^. StaticElement.charRepr)

instance TileRepr Actor where
  toTile a = Tile (asIdentifierOf $ a ^. Actor.id) (a ^. Actor.charRepr)

instance TileRepr Tile where
  toTile = Prelude.id
