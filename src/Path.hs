module Path ( searchPath
            , existsPath
            ) where

import Control.Lens ((&), (%~))

import Data.Default
import Data.Maybe (isJust)

import qualified Data.Map as Map
import qualified Data.PSQueue as PSQ

import Types
import Path.Internal

searchPath :: (Coord -> Bool)
           -> (Coord -> Double)
           -> (Coord -> Coord -> Double)
           -> Coord -> Coord -> Maybe [Coord]
searchPath w h step start goal = fst $ pathFinderSearch w h step start goal

existsPath :: (Coord -> Bool)
           -> (Coord -> Double)
           -> (Coord -> Coord -> Double)
           -> Coord -> Coord -> Bool
existsPath w h step start goal = isJust $ searchPath w h step start goal

-- private helper
pathFinderSearch :: (Coord -> Bool)
           -> (Coord -> Double)
           -> (Coord -> Coord -> Double)
           -> Coord -> Coord -> (Maybe [Coord], PathFinderState)
pathFinderSearch walkable heuristic step start goal =
  runPathFinder config initState $ findPath start goal
  where
    config = PathFinderConfig walkable heuristic step
    initState :: PathFinderState
    initState = def & seen %~ Map.insert start (0,Nothing)
                    & open %~ PSQ.insert start 0