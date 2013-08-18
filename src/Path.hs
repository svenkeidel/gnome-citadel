{-# LANGUAGE TemplateHaskell #-}
module Path ( neighbors
            , findPath

            , PathFinderState (PathFinderState)
            , closed
            , open
            , seen
            ) where

import Data.Monoid as DM
import Control.Lens (both, (%~))
import Control.Lens.TH
import Control.Monad (guard)
import Data.Default

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.PSQueue as PSQ

import Types

type Score = Double

data PathFinderState = PathFinderState { _closed :: Set.Set Coord
                                       , _open :: PSQ.PSQ Coord Score
                                       , _seen :: Map.Map Coord (Score,Coord)
                                       }

instance Default PathFinderState where
  def = PathFinderState def PSQ.empty def

makeLenses ''PathFinderState

findPath :: Coord -> Coord -> [Coord]
findPath = undefined

allowedDirections :: [Coord]
allowedDirections = do
  x <- [-1, 0, 1]
  y <- [-1, 0, 1]
  guard $ (x,y) /= (0,0)
  return (x,y)

neighbors :: Coord -> [Coord]
neighbors c = map (unsumTuple . DM.mappend fromCoordSum) toCoordSums
  where fromCoordSum = sumTuple c
        toCoordSums = map sumTuple allowedDirections

        sumTuple :: Num a => (a,a) -> (Sum a, Sum a)
        sumTuple = both %~ DM.Sum

        unsumTuple :: (Sum a, Sum a) -> (a,a)
        unsumTuple (Sum x, Sum y) = (x,y)