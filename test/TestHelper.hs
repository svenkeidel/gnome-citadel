{-# LANGUAGE RankNTypes #-}
module TestHelper ( createLevel
                  , findDwarf
                  , findWall
                  , isRight
                  , isLeft
                  , levelShouldBe
                  ) where

import Control.Lens((^.),(.~),(&))
import Control.Monad.Error

import Test.Hspec(shouldBe)

import Data.List(find)
import Data.Maybe(isJust)
import Data.Default

import Actor(Actor)
import Counter
import Level
import Level.Transformation (LevelError)
import Renderable
import StaticElement(StaticElement)
import Tile
import TestTiles

createLevel :: String -> Level
createLevel lvlString =
  let (lvl,_) = fromString levelBuilder lvlString def
  in lvl & walkable .~ (\lvl' coord -> not $ contains '#' (lvl' ^. at coord))
  where
    contains x = isJust . find ((x ==) . render)

levelBuilder :: TileBuilder
levelBuilder ident char =
  case char of
    '#' -> Just $ Right $ wall (asIdentifierOf ident)
    ' ' -> Just $ Right $ free (asIdentifierOf ident)
    '@' -> Just $ Left  $ dwarf (asIdentifierOf ident)
    _   -> error ("unrecognized char " ++ show char)

findDwarf :: Level -> Actor
findDwarf lvl = head $ lvl ^. findActor (\a -> render (toTile a) == '@')

findWall :: (Int, Int) -> Level -> StaticElement
findWall c lvl = head $ lvl ^. findStaticElement (\t -> render (toTile t) == '#'
                                                     && lvl ^. coordOf t  == from2d c)

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

isLeft :: Either a b -> Bool
isLeft = not . isRight

levelShouldBe :: [String] -> Level -> ErrorT LevelError IO Level
levelShouldBe s lvl = do
  lift $ show lvl `shouldBe` unlines s
  return lvl
