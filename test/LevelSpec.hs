module LevelSpec(main, spec) where

import Test.Hspec
import Level
import Renderable
import Tile

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "A Level" $ do

    let levelString = unlines
          [ "###"
          , " ##"
          , "@ #"
          ]
        levelBuilder char =
          case char of
              '#' -> Right wall
              '@' -> Left dwarf
              _   -> error ("unrecognized char " ++ show char)
        level = fromString levelBuilder levelString

    it "can be accessed via coordinates" $ do
      render (head $ level `at` (1,1)) `shouldBe` '#'
      render (head $ level `at` (0,2)) `shouldBe` '@'
      render (head $ level `at` (2,0)) `shouldBe` '#'
      null (level `at` (1,2)) `shouldBe` True
      null (level `at` (0,1)) `shouldBe` True

    it "can be rendered to a string" $ do
      pending
      show level `shouldBe` levelString
