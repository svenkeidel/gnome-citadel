{-# LANGUAGE ScopedTypeVariables #-}
module LevelTransSpec(main, spec) where

import Control.Lens((^.), (.~), (&))
import Control.Monad.State
import Control.Monad.Error

import Data.List(find)
import Data.Maybe(isJust)

import Test.Hspec

import Level
import Level.Transformation
import Tile
import Renderable

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "A Level Transformation" $ do

    let levelString = unlines
          [ "## "
          , " # "
          , "@  "
          ]
        levelBuilder char =
          case char of
              '#' -> Just $ Right wall
              ' ' -> Just $ Right free
              '@' -> Just $ Left dwarf
              _   -> error ("unrecognized char " ++ show char)
        contains x = isJust . find ((x ==) . render)
        level  = fromString levelBuilder levelString
               & walkable .~ (\lvl coord -> not $ contains '#' (lvl ^. at coord))
        dwarf' = head $ level ^. findActor (\a -> render (toTile a) == '@')

        levelShouldBe s = do
          lvl <- get
          lift $ lift $ show lvl `shouldBe` unlines s

        isRight (Right _) = True
        isRight _         = False
        isLeft            = not . isRight

    describe "A Move" $ do
      it "moves an actor in one step to an adjacent coordinate" $ do

        e <- runErrorT $ flip execStateT level $ do
             move dwarf' (from2d (1,2))
             levelShouldBe [ "## "
                           , " # "
                           , " @ "
                           ]
             move dwarf' (from2d (0,1))
             levelShouldBe [ "## "
                           , "@# "
                           , "   "
                           ]

        e `shouldSatisfy` isRight

      it "cannot be executed if the destination is blocked" $ do

        e' <- runErrorT $ flip runStateT level $ move dwarf' (from2d (1,1))
        e' `shouldSatisfy` isLeft
