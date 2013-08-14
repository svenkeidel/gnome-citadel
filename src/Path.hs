module Path ( findPath
            , neighbors
            , isBlocked
            , inBounds
            , expand
            ) where

import Data.Monoid as DM
import Control.Lens (both, (%~))

import Level
import Types

type Heuristic a = Coord -> Coord -> a
type Path = [Coord]

findPath :: Coord -> Coord -> Level -> Heuristic Double -> Maybe Path
findPath = undefined

expand :: Coord -> Level -> [Coord]
expand c lvl = filter predicate . neighbors $ c
  where
    predicate :: Coord -> Bool
    predicate = and . conditions

    conditions :: Coord -> [Bool]
    conditions = sequence [ isBlocked lvl
                          , inBounds lvl
                          ]


isBlocked :: Level -> Coord -> Bool
isBlocked = undefined

inBounds :: Level -> Coord -> Bool
inBounds = undefined

allowedDirections :: [Coord]
allowedDirections = straightDirs ++ diagonalDirs

straightDirs :: [Coord]
straightDirs = [ (-1, 0)
               , ( 1, 0)
               , ( 0,-1)
               , ( 0, 1)
               ]

diagonalDirs :: [Coord]
diagonalDirs = [ (-1,-1)
               , (-1, 1)
               , ( 1,-1)
               , ( 1, 1)
               ]

neighbors :: Coord -> [Coord]
neighbors c = map (unsumTuple . DM.mappend fromCoordSum) toCoordSums
  where fromCoordSum = sumTuple c
        toCoordSums = map sumTuple allowedDirections

        sumTuple :: Num a => (a,a) -> (Sum a, Sum a)
        sumTuple = both %~ DM.Sum

        unsumTuple :: (Sum a, Sum a) -> (a,a)
        unsumTuple (Sum x, Sum y) = (x,y)