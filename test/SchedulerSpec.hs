module SchedulerSpec(main, spec) where

import Test.Hspec
import Data.Char(ord)
import Control.Monad.State
import Data.List(isInfixOf,sort)

import Scheduler

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let unfoldList []     = Done
      unfoldList (p:ps) = Yield p ps
      e  = execution [1..10 :: Int] unfoldList
      e' = execution "abcdef" (fmap ord . unfoldList)
      list `shouldContain` subList =
        sort list `shouldSatisfy` (sort subList `isInfixOf`)
  
  describe "A Scheduler" $ do
    it "can register executions" $ do
      evalState (e >> nextSteps) scheduler `shouldBe` [1]
      evalState (e >> nextSteps >> nextSteps) scheduler `shouldBe` [2]
      evalState (e >> e >> nextSteps) scheduler `shouldBe` [1,1]

    it "can handle executions with polymorphic private state" $ do
      evalState (e >> e' >> nextSteps) scheduler
        `shouldContain` [1,97]

    it "can handle multiple walking paths within a level" $ do
      let path1 = [ (   x, y) | x <- [1..10 :: Int], let y = x]
          path2 = [ (10-x, y) | x <- [1..10 :: Int], let y = x]
          walk p = execution p unfoldList
          assertions = do
            walk path1
            walk path2
            s1 <- nextSteps
            lift $ s1 `shouldContain` [(1, 1), (9, 1)]
            s2 <- nextSteps
            lift $ s2 `shouldContain` [(2, 2), (8, 2)]
            s3 <- nextSteps
            lift $ s3 `shouldContain` [(3, 3), (7, 3)]
      execStateT assertions scheduler >> return ()

  describe "A Execution" $ do
    it "can be exhausted" $ do
      evalState (execution [1 :: Int] unfoldList >> nextSteps >> nextSteps) scheduler
        `shouldBe` []
