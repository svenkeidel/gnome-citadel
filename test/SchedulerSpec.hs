module SchedulerSpec(main, spec) where

import Test.Hspec
import Data.Char(ord)
import Control.Monad.State
import Data.List(isInfixOf,sort)

import qualified Scheduler as S
import Unfold

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let e  = S.add $ unfold [1..10 :: Int] unfoldList
      e' = S.add $ unfold "abcdef" (fmap ord . unfoldList)
      list `shouldContain` subList =
        sort list `shouldSatisfy` (sort subList `isInfixOf`)
  
  describe "A Scheduler" $ do
    it "can register executions" $ do
      evalState (e >> S.next) S.empty `shouldBe` [1]
      evalState (e >> S.next >> S.next) S.empty `shouldBe` [2]
      evalState (e >> e >> S.next) S.empty `shouldBe` [1,1]

    it "can handle executions with polymorphic private state" $ do
      evalState (e >> e' >> S.next) S.empty
        `shouldContain` [1,97]

    it "can handle multiple walking paths within a level" $ do
      let path1 = [ (   x, y) | x <- [1..10 :: Int], let y = x]
          path2 = [ (10-x, y) | x <- [1..10 :: Int], let y = x]
          walk p = S.add $ unfold p unfoldList
          assertions = do
            walk path1
            walk path2
            s1 <- S.next
            lift $ s1 `shouldContain` [(1, 1), (9, 1)]
            s2 <- S.next
            lift $ s2 `shouldContain` [(2, 2), (8, 2)]
            s3 <- S.next
            lift $ s3 `shouldContain` [(3, 3), (7, 3)]
      execStateT assertions S.empty >> return ()

  describe "A Execution" $ do
    it "can be exhausted" $ do
      evalState (S.add (unfold [1 :: Int] unfoldList) >> S.next >> S.next) S.empty
        `shouldBe` []
