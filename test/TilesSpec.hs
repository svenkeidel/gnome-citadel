module TilesSpec(main, spec) where

import Test.Hspec
import Counter
import Tile
import TestTiles
import Renderable

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "A Tile" $ do
    it "can be rendered to a char" $ do
      render (toTile $ wall (Identifier 1))  `shouldBe` '#'
      render (toTile $ miner (Identifier 2)) `shouldBe` 'm'
      render (toTile $ free (Identifier 3))  `shouldBe` ' '
