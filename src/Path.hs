module Path ( searchPath
            , existsPath
            , pathCost
            , defaultPath
            ) where

import Control.Lens ((&), (%~), view)

import Data.Default
import Data.Maybe (isJust)

import qualified Data.Map as Map
import qualified Data.PSQueue as PSQ

import Types
import Path.Internal

searchPath :: (Coord -> Bool)            -- ^ Check if coord is allowed
           -> (Coord -> Double)          -- ^ Heuristic value for the coord
           -> (Coord -> Coord -> Double) -- ^ Cost to go from coord to coord
           -> Coord                      -- ^ The start coord
           -> Coord                      -- ^ The goal coord
           -> Maybe Path                 -- ^ Just the path from start to goal or Nothing
searchPath w h step start goal = fst $ pathFinderSearch w h step start goal

existsPath :: (Coord -> Bool)            -- ^ Check if coord is allowed
           -> (Coord -> Double)          -- ^ Heuristic value for the coord
           -> (Coord -> Coord -> Double) -- ^ Cost to go from coord to coord
           -> Coord                      -- ^ The start coord
           -> Coord                      -- ^ The goal coord
           -> Bool                       -- ^ Whether there exists a path from start to goal
existsPath w h step start goal = isJust $ searchPath w h step start goal

pathCost :: (Coord -> Bool)            -- ^ Check if coord is allowed
         -> (Coord -> Double)          -- ^ Heuristic value for the coord
         -> (Coord -> Coord -> Double) -- ^ Cost to go from coord to coord
         -> Coord                      -- ^ The start coord
         -> Coord                      -- ^ The goal coord
         -> Maybe Double               -- ^ Cost of a path from start to goal
pathCost w h step start goal = fmap (view pathLength) (searchPath w h step start goal)

-- | Searches a path from 'start' to 'goal', without walking cells
-- that are not allowed according to the given predicate. The
-- heuristic is assumed to be the euclidean distance.
-- The way cost from a Coord to any of its neighbors is assumed to be always 1
defaultPath :: (Coord -> Bool) -> Coord -> Coord -> Maybe Path
defaultPath predicate start goal =
  searchPath predicate (distance goal) (const . const $ 1) start goal

-- private helper
pathFinderSearch :: (Coord -> Bool)
           -> (Coord -> Double)
           -> (Coord -> Coord -> Double)
           -> Coord
           -> Coord
           -> (Maybe Path, PathFinderState)
pathFinderSearch walkable heuristic step start goal =
  runPathFinder config initState $ findPath start goal
  where
    config = PathFinderConfig walkable heuristic step
    initState :: PathFinderState
    initState = def & seen %~ Map.insert start (0,Nothing)
                    & open %~ PSQ.insert start 0