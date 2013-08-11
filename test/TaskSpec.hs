module TaskSpec(main, spec) where

import Test.Hspec
{-import Test.QuickCheck-}
{-import Types-}
import Level
import Task

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "A Task" $ do
    it "can be assigned to a coordinate" $ do
      let levelString = unlines
            [ "###"
            , " ##"
            , "@ #"
            ]
          level  = fromString levelString
          level' = createTask Mine (1,1) level
      numberOfTasks level  `shouldBe` 0
      numberOfTasks level' `shouldBe` 1
