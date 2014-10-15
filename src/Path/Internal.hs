{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, FlexibleContexts #-}
module Path.Internal ( findPath
                     , expand
                     , visit
                     , analyzeNbs
                     , reconstructPath

                     , ContinueFlooding(..)
                     , floodUntil

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

                     , Path (Path)
                     , pathLength
                     , pathCoords

                     , PredecessorMap
                     ) where

import           Control.Applicative (Applicative, (<*>),(<$>),pure)
import           Control.DeepSeq (NFData (rnf))
import           Control.Lens (view, use)
import           Control.Lens.Operators
import           Control.Lens.TH
import           Control.Monad (when, unless)
import           Control.Monad.Identity (Identity, runIdentity)
import           Control.Monad.Reader.Class
import           Control.Monad.State.Class
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import           Control.Monad.Trans.State (StateT, runStateT)
import           Data.Default
import           Data.Foldable (traverse_)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (isJust,fromJust)
import           Data.PSQueue (Binding((:->)))
import qualified Data.PSQueue as PSQ
import           Data.Set (Set)
import qualified Data.Set as Set

import           Coords

import           Utils(unlessM)

type HeuristicScore = Double
type WayCost = Double

data Path = Path { _pathLength :: !Double
                 , _pathCoords :: [Coord]
                 } deriving (Show,Eq)
makeLenses ''Path

instance NFData Path where
  rnf (Path l cs) = l `seq` cs `seq` ()

type PredecessorMap = Map Coord (WayCost,Maybe Coord)
data PathFinderState = PathFinderState { _closed :: Set Coord
                                       , _open :: PSQ.PSQ Coord HeuristicScore
                                       , _seen :: PredecessorMap
                                       }
makeLenses ''PathFinderState


instance Default PathFinderState where
  def = PathFinderState def PSQ.empty def

data PathFinderConfig = PathFinderConfig { _canBeWalked :: Coord -> Bool
                                         , _heuristicCost :: Coord -> HeuristicScore
                                         , _stepCost :: Coord -> Coord -> WayCost
                                         , _neighbors :: Coord -> [Coord]
                                         , _isGoal :: Coord -> Bool
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

data ContinueFlooding = Continue | Abort deriving (Eq,Show)

floodUntil :: (PredecessorMap -> ContinueFlooding) -> Coord -> PathFinder ()
floodUntil abort current =
  unlessM (use seen <&> abort <&> (== Abort)) $ do
    currentIsWalkable <- view canBeWalked <*> pure current
    if (not currentIsWalkable)
      then do
        nodesLeft <- nodesLeftToExpand
        when (nodesLeft > 0) $ do
          maybeMin <- extractNextMinFromQueue
          traverse_ (floodUntil abort) maybeMin
      else do
        visitNeighbours current
        nodesLeft <- nodesLeftToExpand
        when (nodesLeft > 0) $ do
          maybeMin <- extractNextMinFromQueue
          traverse_ (floodUntil abort) maybeMin

  where
    visitNeighbours c = visit c >> (view neighbors ?? c) >>= analyzeNbs c

findPath :: Coord -> PathFinder (Maybe Path)
findPath current = do
  goalReached <- view isGoal <*> pure current
  if goalReached
    then reconstructPath current <$> use seen
    else do
      visitAndExpand current
      nodesLeft <- nodesLeftToExpand
      ifGreaterZero nodesLeft $ do
        maybeMin <- extractNextMinFromQueue
        case maybeMin of
          Just m -> findPath m
          Nothing -> return Nothing
  where
    ifGreaterZero :: Monad m => Int -> m (Maybe a) -> m (Maybe a)
    ifGreaterZero n action = if n == 0
                               then return Nothing
                               else action

extractNextMinFromQueue :: PathFinder (Maybe Coord)
extractNextMinFromQueue = do
  maybeMinQueue <- PSQ.minView <$> use open
  case maybeMinQueue of
    Just (nextMin :-> _, queue) -> do
      open .= queue
      return $ Just nextMin
    Nothing -> return Nothing

visitAndExpand :: Coord -> PathFinder ()
visitAndExpand c = visit c >> expand c >>= analyzeNbs c

reconstructPath :: Coord -> PredecessorMap -> Maybe Path
reconstructPath finish pmap = do
  (totalCost,predec) <- Map.lookup finish pmap
  case predec of
    Nothing -> return $ Path 0 []
    Just _ -> return $ Path totalCost (reverse $ go finish)
  where
    go :: Coord -> [Coord]
    go current = case Map.lookup current pmap of
      Nothing -> []
      Just (_, Just predec) -> current : go predec
      Just (_, Nothing) -> [current]

nodesLeftToExpand :: PathFinder Int
nodesLeftToExpand = PSQ.size <$> use open

expand :: Coord -> PathFinder [Coord]
expand coord = filter <$> view canBeWalked <*> (view neighbors <*> pure coord)

analyzeNb :: Coord -> Coord -> PathFinder ()
analyzeNb predecessor nb = do
  alreadySeen <- alreadyVisited nb
  costSoFar <- costFor predecessor
  estimation <- view stepCost <*> pure predecessor <*> pure nb
  let newCost = (+) <$> costSoFar <*> pure estimation
  mayUpdateCost newCost nb predecessor
  unless alreadySeen $ do
      heuristicValue <- view heuristicCost <*> pure nb
      when (isJust newCost) $
        open %= insertIfNotPresent nb (fromJust newCost + heuristicValue)

analyzeNbs :: Coord -> [Coord] -> PathFinder ()
analyzeNbs predecessor = mapM_ (analyzeNb predecessor)

costFor :: Coord -> PathFinder (Maybe WayCost)
costFor c = (fmap . fmap) fst $ Map.lookup c <$> use seen

mayUpdateCost :: Maybe WayCost -> Coord -> Coord -> PathFinder ()
mayUpdateCost Nothing _ _ = return ()
mayUpdateCost (Just cost) target origin = do
  previous <- costFor target
  case previous of
    Just previousCost -> when (cost < previousCost) $
      seen %= Map.insert target (cost, Just origin)
    Nothing -> seen %= Map.insert target (cost, Just origin)

insertIfNotPresent :: ( Ord k
                      , Ord p
                      ) => k -> p -> PSQ.PSQ k p -> PSQ.PSQ k p
insertIfNotPresent key prio queue =
  case PSQ.lookup key queue of
    Just _ -> queue
    Nothing -> PSQ.insert key prio queue

visit :: Coord -> PathFinder ()
visit c = closed %= Set.insert c

alreadyVisited :: Coord -> PathFinder Bool
alreadyVisited c = Set.member c <$> use closed
