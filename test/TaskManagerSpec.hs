module TaskManagerSpec(main, spec) where

import Data.Default
import Data.Maybe
import Test.Hspec

import TaskManagement
import Task
import Coords

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "The TaskManager" $ do
    let taskManager = def :: TaskManager
        tId = 1
        task = Task tId (from2d (0,0)) Mine

    context "when adding tasks" $ do
      it "the number of tasks increments when task is reachable" $ do
        let taskManager' = addReachableTask task taskManager
        numberOfTasks taskManager' `shouldBe` 1

      it "the number of tasks increments when task is unreachable" $ do
        let taskManager' = addUnreachableTask task taskManager
        numberOfTasks taskManager' `shouldBe` 1

      it "hasTask returns true only for the added task" $ do
        let taskManager' = addTask True task taskManager
        taskManager' `shouldSatisfy` hasTask tId
        taskManager' `shouldSatisfy` not . hasTask (tId+1)

    context "asking for a task" $ do
      it "should return nothing if the task cannot be found" $ do
        let wrongTaskId = 42
        taskManager `shouldSatisfy` isNothing . getTask wrongTaskId

      it "should return the task if it can be found" $ do
        let taskManager' = addReachableTask task taskManager
        taskManager' `shouldSatisfy` (== Just task) . getTask tId
