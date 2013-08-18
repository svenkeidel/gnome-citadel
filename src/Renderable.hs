{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Renderable ( Renderable(..)
                  ) where

import Tile

import Control.Lens hiding (Level)

class Renderable t repr where
  render :: t -> repr

instance Renderable Tile Char where
  render = view tileCharRepr

instance Renderable [Tile] Char where
  render []    = ' '
  render (t:_) = render t
