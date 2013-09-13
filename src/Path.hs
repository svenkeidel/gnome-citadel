module Path ( searchPath
            , existsPath
            , pathCost
            , defaultPath
            , findArea

            , Path (Path)
            , pathLength
            , pathCoords
            ) where

import Control.Lens ((&), (%~), view)

import Data.Default
import Data.Maybe (isJust)

import qualified Data.Map as Map
import qualified Data.PSQueue as PSQ

import Coords
import Path.Internal

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
pathCost w h step start goal = fmap (view pathLength) (searchPath w h step start (== goal))

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
  searchPath walkable heuristic (const . const $ 1) start (`elem` goals)
  where
    heuristic :: Coord -> Double
    heuristic coord = minimum $ map (distance coord) goals

-- private helper
pathFinderSearch :: (Coord -> Bool)
                 -> (Coord -> Double)
                 -> (Coord -> Coord -> Double)
                 -> Coord
                 -> (Coord -> Bool)
                 -> (Maybe Path, PathFinderState)
pathFinderSearch walkable heuristic step start isGoal =
  runPathFinder config initState $ findPath start
  where
    config = PathFinderConfig walkable heuristic step neighbors2d isGoal
    initState :: PathFinderState
    initState = def & seen %~ Map.insert start (0,Nothing)
                    & open %~ PSQ.insert start 0