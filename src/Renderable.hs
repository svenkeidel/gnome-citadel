{-# LANGUAGE MultiParamTypeClasses #-}
module Renderable ( Renderable(..)
                  ) where

import Tile

import Control.Lens hiding (Level)

class Renderable t repr where
  render :: t -> repr

instance Renderable Tile Char where
  render = view tileCharRepr
