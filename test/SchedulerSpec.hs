{-# LANGUAGE FlexibleContexts #-}
module SchedulerSpec(main, spec) where

import           Control.Monad.State
import           Data.Char(ord)
import           Data.List (sort)
import           Test.Hspec

import qualified Scheduler as S
import           Unfold

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let e  = modify (S.add (unfold [1..10 :: Int] unfoldList))
      e' = modify (S.add (unfold "abcdef" (fmap ord . unfoldList)))
      next' :: Monad m => StateT (S.Scheduler Unfold a) m [a]
      next' = state S.next

  describe "A Scheduler" $ do
    it "can register executions" $ do
      evalState (e >> next') S.empty `shouldBe` [1]
      evalState (e >> next' >> next') S.empty `shouldBe` [2]
      evalState (e >> e >> next') S.empty `shouldBe` [1,1]

    it "can handle executions with polymorphic private state" $ do
      evalState (e >> e' >> next') S.empty
        `shouldContain` [1,97]

    it "can handle multiple walking paths within a level" $ do
      let path1 = [ (   x, y) | x <- [1..10 :: Int], let y = x]
          path2 = [ (10-x, y) | x <- [1..10 :: Int], let y = x]
          walk p = modify (S.add (unfold p unfoldList))
          assertions = do
            walk path1
            walk path2
            s1 <- next'
            lift $ s1 `shouldContain` [(1, 1), (9, 1)]
            s2 <- next'
            lift $ sort s2 `shouldContain` [(2, 2), (8, 2)]
            s3 <- next'
            lift $ s3 `shouldContain` [(3, 3), (7, 3)]
      void $ execStateT assertions S.empty

  describe "A Execution" $ do
    it "can be exhausted" $ do
      evalState (modify (S.add (unfold [1 :: Int] unfoldList)) >> next' >> next') S.empty
        `shouldBe` []
