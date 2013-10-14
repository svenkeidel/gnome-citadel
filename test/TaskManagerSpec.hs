module TaskManagerSpec(main, spec) where
{-
import Control.Lens((^.),(.=),use)
import Control.Monad.State
import Control.Monad.Error

import Data.Maybe

import Level
import TaskManagement
import Task
import TestHelper
-}

import Test.Hspec


main :: IO ()
main = hspec spec


spec :: Spec
spec = describe "The TaskManager" $ do
  return ()
{-
  let lvl = createLevel $
              unlines [ "## "
                      , " # "
                      , "@  "
                      ]
      tskMgr = taskManager lvl
      tId = 1
      task = mine (from2d (1,0))
      everything _ _ = True

      taskManagerShouldSatisfy x = do
        tskMgr' <- get
        lift $ tskMgr' `shouldSatisfy` x

      levelShouldBe s = do
        lvl' <- use level
        lift $ lift $ show lvl' `shouldBe` s

      gameStepShouldChangeLevelTo s = do
        executeGameStep
        levelShouldBe (unlines s)

  context "asking for a task" $ do
    it "should return nothing if the task cannot be found" $ do
      let wrongTaskId = 42
      (tskMgr ^. getTask wrongTaskId) `shouldSatisfy` isNothing

  context "when adding tasks" $ do
    it "hasTask returns true only for the added task" $ do
      void $ flip runTaskManager lvl $ do
        reachable .= everything
        addTask task
        taskManagerShouldSatisfy (^. hasTask tId)
        taskManagerShouldSatisfy (\lvl' -> not $ lvl' ^. hasTask (tId+1))

    it "gets assigned to a dwarf and executed" $ do

      pending

      e <- runErrorT $ flip runTaskManager lvl $ do

        reachable .= everything

        addTask task

        assignTasks

        gameStepShouldChangeLevelTo [ "## "
                                    , "@# "
                                    , "   "
                                    ]

        gameStepShouldChangeLevelTo [ "#@ "
                                    , " # "
                                    , "   "
                                    ]


        gameStepShouldChangeLevelTo [ "#@ "
                                    , " # "
                                    , "   "
                                    ]

      e `shouldSatisfy` isRight
-}
