module TestHelper ( createLevel
                  , findDwarf
                  , findDwarfByCoord
                  , findWall
                  , isRight
                  , isLeft
                  ) where

import Control.Lens.Operators

import Data.List(find)
import Data.Maybe(isJust)
import Data.Default

import Actor(Actor)
import Counter
import Level
import Renderable
import StaticElement(StaticElement)
import Tile
import TestTiles

createLevel :: String -> Level
createLevel lvlString = lvl & walkable .~ walkableFunction
  where
    lvl = fst $ fromString levelBuilder lvlString def
    contains x = isJust . find ((x ==) . render)
    walkableFunction level coord = (not . contains '#' . at coord $ level)
                                   && inBounds coord level

levelBuilder :: TileBuilder
levelBuilder ident char =
  case char of
    '#' -> Just $ Right $ wall (asIdentifierOf ident)
    ' ' -> Just $ Right $ free (asIdentifierOf ident)
    'm' -> Just $ Left  $ miner (asIdentifierOf ident)
    'c' -> Just $ Left  $ chopper (asIdentifierOf ident)
    _   -> error ("unrecognized char " ++ show char)

findDwarf :: Char -> Level -> Actor
findDwarf c = head . findActor (\a -> render (toTile a) == c)

findDwarfByCoord :: Coord -> Level -> Actor
findDwarfByCoord c = head . actorsAt c

findWall :: (Int, Int) -> Level -> StaticElement
findWall c lvl = head $ findStaticElement (\t -> render (toTile t) == '#'
                                                 && lvl ^. coordOf t  == from2d c) lvl

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

isLeft :: Either a b -> Bool
isLeft = not . isRight
