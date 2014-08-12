{-# LANGUAGE TemplateHaskell #-}
module Coords ( Coord (Coord)
              , cx
              , cy
              , cz
              , from2d
              , to2d
              , distance
              , neighbors2d

              , SumCoord (..)
              , (|+|)
              ) where

import Control.Lens.TH
import Data.Monoid
import Data.Function(on)
import Control.Monad (guard)

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

newtype SumCoord = SumCoord { getSumCoord :: Coord } deriving Show
makeLenses ''SumCoord

instance Monoid SumCoord where
  mempty = SumCoord $ Coord 0 0 0
  mappend (SumCoord (Coord x1 y1 z1)) (SumCoord (Coord x2 y2 z2)) =
    SumCoord $ Coord (x1+x2) (y1+y2) (z1+z2)

infixl 6 |+|
(|+|) :: Coord -> Coord -> Coord
c1 |+| c2 = getSumCoord $ on mappend SumCoord c1 c2

from2d :: (Int,Int) -> Coord
from2d (x,y) = Coord x y 0

to2d :: Coord -> (Int,Int)
to2d (Coord x y _) = (x,y)

directions2d :: [Coord]
directions2d = do
  x <- [-1, 0, 1]
  y <- [-1, 0, 1]
  guard $ (x,y) /= (0,0)
  return . from2d $ (x,y)

neighbors2d :: Coord -> [Coord]
neighbors2d c = [c |+| dir | dir <- directions2d]
