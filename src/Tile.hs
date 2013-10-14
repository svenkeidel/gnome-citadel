{-# LANGUAGE TemplateHaskell, EmptyDataDecls #-}
module Tile ( Tile(..)
            , Tile.id
            , charRepr
            , TileRepr(..)
            ) where

import Types
import Control.Applicative
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
  toTile = Tile <$> view StaticElement.id <*> view StaticElement.charRepr

instance TileRepr Actor where
  toTile = Tile <$> view Actor.id <*> view Actor.charRepr

instance TileRepr Tile where
  toTile = Prelude.id
