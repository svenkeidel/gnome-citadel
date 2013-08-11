module TaskSpec(main, spec) where

import Test.Hspec
{-import Test.QuickCheck-}
{-import Types-}
import Level
import Task

import Data.Ord (comparing)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "A Task" $ do
    let levelString = unlines
            [ "###"
            , " ##"
            , "@ #"
            ]
        level = fromString levelString

    it "can be added to a level" $ do
      let (identifier,level') = createTask Mine (1,1) level
      hasTask identifier level' `shouldBe` True
      comparing numberOfTasks level' level `shouldBe` GT

    it "can be assigned to a coordinate" $ do
      let (identifier,level') = createTask Mine (1,1) level
      (fst . getTask identifier $ level') `shouldBe` (1,1)
