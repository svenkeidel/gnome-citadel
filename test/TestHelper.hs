{-# LANGUAGE RankNTypes #-}
module TestHelper ( createLevel
                  , findDwarf
                  , findWall
                  , isRight
                  , isLeft
                  ) where

import Control.Lens((^.),(.~),(&))
import qualified Control.Lens.Getter as LG

import Data.List(find)
import Data.Maybe(isJust)

import Actor(Actor)
import Level
import Renderable
import StaticElement(StaticElement)
import Tile
import TestTiles

createLevel :: String -> Level
createLevel lvlString = fromString levelBuilder lvlString
                      & walkable .~ (\lvl coord -> not $ contains '#' (lvl ^. at coord))
  where
    contains x = isJust . find ((x ==) . render)

levelBuilder :: TileBuilder
levelBuilder char =
  case char of
    '#' -> Just $ Right wall
    ' ' -> Just $ Right free
    '@' -> Just $ Left dwarf
    _   -> error ("unrecognized char " ++ show char)

findDwarf :: LG.Getter Level Actor
findDwarf = LG.to $ \lvl ->
  head $ lvl ^. findActor (\a -> render (toTile a) == '@')

findWall :: (Int, Int) -> LG.Getter Level StaticElement
findWall c = LG.to $ \lvl ->
  head $ lvl ^. findStaticElement (\t -> render (toTile t) == '#'
                                      && lvl ^. coordOf t  == from2d c)

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

isLeft :: Either a b -> Bool
isLeft = not . isRight
