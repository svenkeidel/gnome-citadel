module LevelSpec(main, spec) where

import Test.Hspec

import Level
import Renderable
import TestHelper

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "A Level" $ do

  let levelString = unlines
        [ "###"
        , " ##"
        , "m #"
        ]
      level = createLevel levelString
      at' c = head $ at (from2d c) level

  it "can be accessed via coordinates" $ do
    render (at' (1,1)) `shouldBe` '#'
    render (at' (0,2)) `shouldBe` 'm'
    render (at' (2,0)) `shouldBe` '#'
    render (at' (0,1)) `shouldBe` ' '
    render (at' (1,2)) `shouldBe` ' '

  it "can be rendered to a string" $ do
    show level `shouldBe` levelString
