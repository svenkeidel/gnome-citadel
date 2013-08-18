{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Path ( neighbors
            , findPath

            , PathFinderState (PathFinderState)
            , closed
            , open
            , seen

            , PathFinderConfig (PathFinderConfig)
            , canBeWalked
            , heuristicCost

            , PathFinder
            , runPathFinder
            ) where

import Data.Monoid as DM
import Control.Lens (both, (%~))
import Control.Lens.TH
import Data.Default

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Identity
import Control.Monad.Reader.Class
import Control.Monad.State.Class

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.PSQueue as PSQ

import Types

type Score = Double

data PathFinderState = PathFinderState { _closed :: Set.Set Coord
                                       , _open :: PSQ.PSQ Coord Score
                                       , _seen :: Map.Map Coord (Score,Coord)
                                       }
makeLenses ''PathFinderState

instance Default PathFinderState where
  def = PathFinderState def PSQ.empty def

data PathFinderConfig = PathFinderConfig { _canBeWalked :: Coord -> Bool
                                         , _heuristicCost :: Coord -> Coord -> Score
                                         }
makeLenses ''PathFinderConfig

newtype PathFinder a = PathFinder (ReaderT PathFinderConfig (
                                      StateT PathFinderState Identity) a)
    deriving (Functor, Monad, MonadState PathFinderState, MonadReader PathFinderConfig)

runPathFinder :: PathFinderConfig -> PathFinderState -> PathFinder a -> (a,PathFinderState)
runPathFinder c st (PathFinder a) = runIdentity $ runStateT (runReaderT a c) st

findPath :: Coord -> Coord -> PathFinder [Coord]
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