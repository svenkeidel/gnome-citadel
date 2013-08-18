module PathSpec(main, spec) where

import Test.Hspec
import Path
import Data.List (sort)
import Data.Default

import Control.Lens

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Path finding functionality" $ do
  let pfConf = PathFinderConfig (const True) (const . const $ 0)
      pfState = def :: PathFinderState

  {-
            (4,4)     (5,4)      (6,4)
                        |
                        |
                        |
            (4,5)-----(5,5)------(6,5)
                        |
                        |
                        |
            (4,6)     (5,6)      (6,6)
  -}
  it "should retrieve all possible neighbor coords" $
    (sort . neighbors) (5,5) `shouldBe` sort [ (4,4),(5,4),(6,4)
                                             , (4,5),      (6,5)
                                             , (4,6),(5,6),(6,6)
                                             ]

  it "should not expand to disallowed coords" $ do
    let conf = pfConf & canBeWalked .~ (/= (0,0))
        (result,_) = runPathFinder conf pfState $ expand (0,1)
    (0,0) `elem` result `shouldBe` False

  it "should add the cell to closed set after visiting it" $ do
    let ((b,a),_) = runPathFinder pfConf pfState $ do
          let cell = (0,0)
          before <- alreadyVisited cell
          visit cell
          after <- alreadyVisited cell
          return (before,after)
    (b,a) `shouldBe` (False,True)