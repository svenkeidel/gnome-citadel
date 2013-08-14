module PathSpec(main, spec) where

import Test.Hspec
import Path
import Data.List (sort)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Path finding functionality" $ do

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