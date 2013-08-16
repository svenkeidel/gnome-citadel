module LevelSpec(main, spec) where

import Test.Hspec
import Level
import Renderable

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "A Level" $ do
    it "can be accessed via coordinates" $ do
      pending
      let levelString = unlines
            [ "###"
            , " ##"
            , "@ #"
            ]
          level = fromString levelString
      render (head $ level `at` (1,1)) `shouldBe` '#'
      render (head $ level `at` (0,2)) `shouldBe` '@'
      render (head $ level `at` (1,2)) `shouldBe` ' '
      render (head $ level `at` (2,0)) `shouldBe` '#'
