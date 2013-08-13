module LevelSpec(main, spec) where

import Test.Hspec
import Level
import Renderable

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "A Level" $ do
    it "can be accessed via coordinates" $ do
      let levelString = unlines
            [ "###"
            , " ##"
            , "@ #"
            ]
          level = fromString levelString
      render (level `at` (1,1)) `shouldBe` '#'
      render (level `at` (0,2)) `shouldBe` '@'
      render (level `at` (1,2)) `shouldBe` ' '
      render (level `at` (2,0)) `shouldBe` '#'
