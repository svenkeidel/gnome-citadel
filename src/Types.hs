{-# LANGUAGE TemplateHaskell #-}
module Types ( Identifier
             , Coord (Coord)
             , cx
             , cy
             , cz
             , from2d
             , to2d

             , SumCoord (..)
             ) where

import Control.Lens.TH

import Data.Monoid

type Identifier = Int

data Coord = Coord { _cx :: Int, _cy :: Int, _cz :: Int } deriving (Show,Eq,Ord)
makeLenses ''Coord

newtype SumCoord = SumCoord { getSumCoord :: Coord }
instance Monoid SumCoord where
  mempty = SumCoord $ Coord 0 0 0
  mappend (SumCoord (Coord x1 y1 z1)) (SumCoord (Coord x2 y2 z2)) =
    SumCoord $ Coord (x1+x2) (y1+y2) (z1+z2)

from2d :: (Int,Int) -> Coord
from2d (x,y) = Coord x y 0

to2d :: Coord -> (Int,Int)
to2d (Coord x y _) = (x,y)