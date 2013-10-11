module LevelSpec(main, spec) where

import Control.Lens ((^.))
import Control.Monad.State

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
        , "@ #"
        ]
      level = createLevel levelString
      at' c = head $ level ^. at (from2d c)

  it "can be accessed via coordinates" $ do
    render (at' (1,1)) `shouldBe` '#'
    render (at' (0,2)) `shouldBe` '@'
    render (at' (2,0)) `shouldBe` '#'
    render (at' (0,1)) `shouldBe` ' '
    render (at' (1,2)) `shouldBe` ' '

  it "can generate fresh identifiers" $ do
    let ((n1, n2, n3), level') = runState generateFreshIdentifiers level
        generateFreshIdentifiers = do
          n1' <- freshId
          n2' <- freshId
          n3' <- freshId
          return (n1', n2', n3')
    n2 `shouldBe` n1 + 1
    n3 `shouldBe` n2 + 1
    level' ^. nextFreeId `shouldBe` n3

  it "can be rendered to a string" $ do
    show level `shouldBe` levelString
