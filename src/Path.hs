{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, FlexibleContexts #-}
module Path ( neighbors
            , findPath
            , expand
            , expandUnvisited
            , visit

            , PathFinderState (PathFinderState)
            , closed
            , open
            , seen
            , alreadyVisited

            , PathFinderConfig (PathFinderConfig)
            , canBeWalked
            , heuristicCost

            , PathFinder
            , runPathFinder
            ) where

import Data.Monoid as DM
import Control.Lens (both, (%~), view, (%=))
import Control.Lens.TH
import Data.Default
import Control.Applicative (Applicative, (<*>),(<$>),pure)

import Control.Monad(guard, filterM, (<=<))

import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (StateT, runStateT)
import Control.Monad.Identity (Identity, runIdentity)
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
    deriving ( Functor, Applicative, Monad
             , MonadState PathFinderState, MonadReader PathFinderConfig)

runPathFinder :: PathFinderConfig ->
                 PathFinderState ->
                 PathFinder a ->
                 (a,PathFinderState)
runPathFinder c st (PathFinder a) = runIdentity $ runStateT (runReaderT a c) st

findPath :: Coord -> Coord -> PathFinder [Coord]
findPath = undefined

expand :: (Applicative m, MonadReader PathFinderConfig m) => Coord -> m [Coord]
expand coord = filter <$> asks (view canBeWalked) <*> pure (neighbors coord)

visit :: MonadState PathFinderState m => Coord -> m ()
visit c = closed %= Set.insert c

alreadyVisited :: (Functor m, MonadState PathFinderState m) => Coord -> m Bool
alreadyVisited c = Set.member c <$> gets (view closed)

expandUnvisited :: ( Applicative m
                   , MonadReader PathFinderConfig m
                   , MonadState PathFinderState m
                   ) => Coord -> m [Coord]
expandUnvisited coord =
  filterM (fmap not . alreadyVisited) <=< expand $ coord

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