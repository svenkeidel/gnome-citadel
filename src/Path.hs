module Path ( searchPath
            ) where

import Control.Lens ((&), (%~))
import Data.Default

import qualified Data.Map as Map
import qualified Data.PSQueue as PSQ

import Types
import Path.Internal

searchPath :: (Coord -> Bool)
           -> (Coord -> Double)
           -> (Coord -> Coord -> Double)
           -> Coord -> Coord -> Maybe [Coord]
searchPath walkable heuristic step start goal =
  evalPathFinder config initState $ findPath start goal
  where
    config = PathFinderConfig walkable heuristic step
    initState :: PathFinderState
    initState = def & seen %~ Map.insert start (0,Nothing)
                    & open %~ PSQ.insert start 0