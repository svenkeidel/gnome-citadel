module TilesSpec(main, spec) where

import Test.Hspec
import Tile
import TestTiles
import Renderable

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "A Tile" $ do
    it "can be rendered to a char" $ do
      render (toTile wall)  `shouldBe` '#'
      render (toTile dwarf) `shouldBe` '@'
      render (toTile free)  `shouldBe` ' '
