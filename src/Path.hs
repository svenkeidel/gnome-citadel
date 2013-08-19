{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, FlexibleContexts #-}
module Path ( neighbors
            , findPath
            , expand
            , expandUnvisited
            , visit
            , analyzeNbs
            , costFor
            , reconstructPath

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
            , evalPathFinder
            , execPathFinder
            ) where

import qualified Data.Monoid as DM
import Control.Lens (view, (%=), use, (.=))
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

type PredecessorMap = Map.Map Coord (Score,Maybe Coord)
data PathFinderState = PathFinderState { _closed :: Set.Set Coord
                                       , _open :: PSQ.PSQ Coord Score
                                       , _seen :: PredecessorMap
                                       }
makeLenses ''PathFinderState

instance Default PathFinderState where
  def = PathFinderState def PSQ.empty def

data PathFinderConfig = PathFinderConfig { _canBeWalked :: Coord -> Bool
                                         , _heuristicCost :: Coord -> Score
                                         , _stepCost :: Coord -> Coord -> Score
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

execPathFinder :: PathFinderConfig ->
                  PathFinderState ->
                  PathFinder a ->
                  PathFinderState
execPathFinder c st a = snd $ runPathFinder c st a

evalPathFinder :: PathFinderConfig ->
                  PathFinderState ->
                  PathFinder a ->
                  a
evalPathFinder c st a = fst $ runPathFinder c st a

findPath :: Coord -> Coord -> PathFinder (Maybe [Coord])
findPath current goal =
  if targetFound
    then (Just . reverse . reconstructPath goal) <$> use seen
    else do
      nodesLeft <- nodesLeftToExpand
      ifGreaterZero nodesLeft $ do
        visit current
        nbs <- expand current
        current `analyzeNbs` nbs
        next <- do
          Just (k PSQ.:-> _, queue) <- PSQ.minView <$> use open
          open .= queue
          return k
        findPath next goal
  where
    targetFound = current == goal
    ifGreaterZero :: Monad m => Int -> m (Maybe a) -> m (Maybe a)
    ifGreaterZero n action = if n == 0
                               then return Nothing
                               else action

reconstructPath :: Coord -> PredecessorMap -> [Coord]
reconstructPath finish pmap = case Map.lookup finish pmap of
  Nothing -> []
  Just (_, Just predec) -> finish : reconstructPath predec pmap
  Just (_, Nothing) -> [finish]

nodesLeftToExpand :: (Functor m, MonadState PathFinderState m) => m Int
nodesLeftToExpand = PSQ.size <$> gets (view open)

expand :: (Applicative m, MonadReader PathFinderConfig m) => Coord -> m [Coord]
expand coord = filter <$> asks (view canBeWalked) <*> pure (neighbors coord)

analyzeNb :: ( Applicative m
              , MonadReader PathFinderConfig m
              , MonadState PathFinderState m
              ) => Coord -> Coord -> m ()
analyzeNb predecessor nb = do
  wasVisited <- alreadyVisited nb
  costSoFar <- costFor predecessor
  estimation <- view stepCost <*> pure predecessor <*> pure nb
  previousCost <- costFor nb
  let newCost = (+estimation) <$> costSoFar
      betterWayFound = (<) <$> newCost <*> previousCost
  if wasVisited
    then case betterWayFound of
        Just True -> updateCost newCost nb predecessor
        _ -> return ()
    else
      case newCost of
        Just cost -> do
          seen %= Map.insert nb (cost,Just predecessor)
          heuristicValue <- view heuristicCost <*> pure nb
          open %= insertIfNotPresent nb heuristicValue
        Nothing -> return ()

analyzeNbs :: ( Applicative m
              , MonadReader PathFinderConfig m
              , MonadState PathFinderState m
              ) => Coord -> [Coord] -> m ()
analyzeNbs predecessor = mapM_ (analyzeNb predecessor)

costFor :: (Functor m, MonadState PathFinderState m) => Coord -> m (Maybe Score)
costFor c = (fmap . fmap) fst $ Map.lookup c <$> use seen

updateCost :: (MonadState PathFinderState m) =>
              Maybe Score -> Coord -> Coord -> m ()
updateCost (Just cost) nb p = seen %= Map.insert nb (cost, Just p)
updateCost Nothing _ _ = return ()

insertIfNotPresent :: (Ord k, Ord p) => k -> p -> PSQ.PSQ k p -> PSQ.PSQ k p
insertIfNotPresent key prio queue = case PSQ.lookup key queue of
  Just _ -> queue
  Nothing -> PSQ.insert key prio queue

visit :: MonadState PathFinderState m => Coord -> m ()
visit c = closed %= Set.insert c

alreadyVisited :: (Functor m, MonadState PathFinderState m) => Coord -> m Bool
alreadyVisited c = Set.member c <$> use closed

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
  return . from2d $ (x,y)

neighbors :: Coord -> [Coord]
neighbors c = map (getSumCoord . DM.mappend fromCoordSum) toSumCoords
  where fromCoordSum = SumCoord c
        toSumCoords = map SumCoord allowedDirections
