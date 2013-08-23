{-# LANGUAGE TemplateHaskell #-}
module Coords ( Coord (Coord)
              , cx
              , cy
              , cz
              , from2d
              , to2d
              , distance

              , SumCoord (..)
              ) where

import Control.Lens.TH
import Data.Monoid

data Coord = Coord { _cx :: Int, _cy :: Int, _cz :: Int } deriving (Eq,Ord)
makeLenses ''Coord

distance :: Coord -> Coord -> Double
distance (Coord x1 y1 z1) (Coord x2 y2 z2) = sqrt $ xSum + ySum + zSum
  where
    xSum = fromIntegral . square $ x1 - x2
    ySum = fromIntegral . square $ y1 - y2
    zSum = fromIntegral . square $ z1 - z2
    square x = x * x

instance Show Coord where
  show (Coord x y z) = show (x,y,z)

newtype SumCoord = SumCoord { getSumCoord :: Coord }
instance Monoid SumCoord where
  mempty = SumCoord $ Coord 0 0 0
  mappend (SumCoord (Coord x1 y1 z1)) (SumCoord (Coord x2 y2 z2)) =
    SumCoord $ Coord (x1+x2) (y1+y2) (z1+z2)

from2d :: (Int,Int) -> Coord
from2d (x,y) = Coord x y 0

to2d :: Coord -> (Int,Int)
to2d (Coord x y _) = (x,y)