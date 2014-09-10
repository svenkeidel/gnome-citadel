module Path ( searchPath
            , existsPath
            , pathCost
            , defaultPath
            , findArea
            , floodUntil

            , Path (Path)
            , P.pathLength
            , P.pathCoords
            ) where

import Control.Lens ((&), (%~), view)

import Data.Default
import Data.Maybe (isJust)

import qualified Data.Map as Map
import qualified Data.PSQueue as PSQ

import Coords
import Path.Internal (Path, PathFinderState, PredecessorMap)
import qualified Path.Internal as P

searchPath :: (Coord -> Bool)            -- ^ Check if coord is allowed
           -> (Coord -> Double)          -- ^ Heuristic value for the coord
           -> (Coord -> Coord -> Double) -- ^ Cost to go from coord to coord
           -> Coord                      -- ^ The start coord
           -> (Coord -> Bool)            -- ^ Check if current coord is the goal
           -> Maybe Path                 -- ^ Just the path from start to goal or Nothing
searchPath w h step start isGoal = fst $ pathFinderSearch w h step start isGoal

existsPath :: (Coord -> Bool)            -- ^ Check if coord is allowed
           -> (Coord -> Double)          -- ^ Heuristic value for the coord
           -> (Coord -> Coord -> Double) -- ^ Cost to go from coord to coord
           -> Coord                      -- ^ The start coord
           -> Coord                      -- ^ The goal coord
           -> Bool                       -- ^ Whether there exists a path from start to goal
existsPath w h step start goal = isJust $ searchPath w h step start (== goal)

pathCost :: (Coord -> Bool)            -- ^ Check if coord is allowed
         -> (Coord -> Double)          -- ^ Heuristic value for the coord
         -> (Coord -> Coord -> Double) -- ^ Cost to go from coord to coord
         -> Coord                      -- ^ The start coord
         -> Coord                      -- ^ The goal coord
         -> Maybe Double               -- ^ Cost of a path from start to goal
pathCost w h step start goal = fmap (view P.pathLength) (searchPath w h step start (== goal))

-- | Searches a path from 'start' to 'goal', without walking cells
-- that are not allowed according to the given predicate. The
-- heuristic is assumed to be the euclidean distance.
-- The way cost from a Coord to any of its neighbors is assumed to be always 1
defaultPath :: (Coord -> Bool) -> Coord -> Coord -> Maybe Path
defaultPath walkable start goal =
  searchPath walkable (distance goal) (const . const $ 1) start (== goal)

-- | Finds a path to a nearest coordinate that is part of the
-- specified area.
findArea :: (Coord -> Bool) -> Coord -> [Coord] -> Maybe Path
findArea _ _ [] = Nothing
findArea walkable start goals =
  searchPath walkable heuristic distance start (`elem` goals)
  where
    heuristic :: Coord -> Double
    heuristic coord = minimum $ map (distance coord) goals

floodUntil :: (PredecessorMap -> Bool) -> (Coord -> Bool) -> Coord -> PredecessorMap
floodUntil p walkable start =
  view P.seen . P.execPathFinder config (initState start) $ P.floodUntil p start
  where config = P.PathFinderConfig walkable (const 1) distance neighbors2d (const False)

-- private helper
pathFinderSearch :: (Coord -> Bool)
                 -> (Coord -> Double)
                 -> (Coord -> Coord -> Double)
                 -> Coord
                 -> (Coord -> Bool)
                 -> (Maybe Path, PathFinderState)
pathFinderSearch walkable heuristic step start isGoal =
  P.runPathFinder config (initState start) $ P.findPath start
  where
    config = P.PathFinderConfig walkable heuristic step neighbors2d isGoal

-- Insert start into open set, set predecessor of start to Nothing
initState :: Coord -> PathFinderState
initState start = def & P.seen %~ Map.insert start (0,Nothing)
                      & P.open %~ PSQ.insert start 0
