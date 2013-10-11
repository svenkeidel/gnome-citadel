module TaskManagerSpec(main, spec) where

import Control.Lens((^.),(.=))
import Control.Monad.State

import Data.Maybe

import Test.Hspec

import Level
import TaskManagement
import Task

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "The TaskManager" $ do

  let lvl = emptyLevel
      tskMgr = taskManager lvl
      tId = 1
      task = mine (from2d (1,0))

  context "when adding tasks" $ do
    it "hasTask returns true only for the added task" $ do
      void $ flip runTaskManager lvl $ do
        reachable .= (\_ _ -> True)
        addTask task
        tskMgr' <- get
        lift $ tskMgr' ^. hasTask tId     `shouldBe` True
        lift $ tskMgr' ^. hasTask (tId+1) `shouldBe` False

  context "asking for a task" $ do
    it "should return nothing if the task cannot be found" $ do
      let wrongTaskId = 42
      (tskMgr ^. getTask wrongTaskId) `shouldSatisfy` isNothing
