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
  let e  = execution [2..10 :: Int] (\s (p:ps) -> if s `mod` p == 0 then Yield p ps else Done)
      e' = execution "abcdefgh" (\s (p:ps) -> if ord p == s then Yield s ps else Done)
  
  describe "A Scheduler" $ do
    it "can register executions" $ do
      evalState (e >> nextSteps 4) scheduler `shouldBe` [2]
      evalState (e >> nextSteps 6) scheduler `shouldBe` [2]
      evalState (e >> e >> nextSteps 4) scheduler `shouldBe` [2,2]

    it "can handle executions with polymorphic private state" $ do
      evalState (e >> e' >> nextSteps 97) scheduler
        `shouldBe` [97]

    it "can handle multiple walking paths within a level" $ do
      let path1 = [ (x, y) | x <- [1..10 :: Int], let y = x]
          path2 = [ (3, y) | y <- [2..10 :: Int]]
          walk path = execution path (\s (p:ps) -> if p `elem` s then Done else Yield p ps)
          list `shouldContain` subList =
            sort list `shouldSatisfy` (sort subList `isInfixOf`)
          assertions = do
            walk path1
            walk path2
            s1 <- nextSteps [(0,0), (3, 1)]
            lift $ s1 `shouldContain` [(1, 1), (3, 2)]
            s2 <- nextSteps s1
            lift $ s2 `shouldContain` [(2, 2), (3, 3)]
            s3 <- nextSteps s2
            lift $ s3 `shouldContain` [(3, 4)]
            
      execStateT assertions scheduler >> return ()

  describe "A Execution" $ do
    it "can be exhausted" $ do
      evalState (e >> nextSteps 4 >> nextSteps 4) scheduler
        `shouldBe` []

      
