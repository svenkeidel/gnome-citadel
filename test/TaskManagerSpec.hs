module TaskManagerSpec(main, spec) where

import Control.Monad.Error
import Control.Lens (_1)

import Counter
import Level
import TaskManagement

import qualified Level.Task as LevelTask
import qualified Level.Scheduler as S
import Level.Transformation

import TestHelper
import Test.Hspec

type TaskManagerState = (Level, S.CommandScheduler, TaskManager)
type TaskManagerStateE = ErrorT LevelError IO TaskManagerState

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "The TaskManager" $ do
  let lvl = createLevel $ unlines [ "## "
                                  , " # "
                                  , "@  "
                                  ]
      task = LevelTask.mine (findWall (1,0) lvl) lvl (Identifier 1)

      executeGameStep' :: TaskManagerState -> TaskManagerStateE
      executeGameStep' (lvl', sched, tm) = ErrorT . return $ fmap (\(a,b) -> (a,b,tm)) (S.executeGameStep (lvl',sched))

      gameStepShouldChangeLevelTo :: [String] -> TaskManagerState -> TaskManagerStateE
      gameStepShouldChangeLevelTo s =
        executeGameStep' >=> mapLevel (levelShouldBe s)

      mapLevel :: Functor m => (Level -> m Level) -> TaskManagerState -> m TaskManagerState
      mapLevel = _1

  context "asking for a task" $
    it "should return nothing if the task cannot be found"
      pending

  context "when adding tasks" $ do
    let taskManagerWithTask = addTask task taskManager
        (cmdSchedAssigned, taskManagerAssigned) =
          assignTasks lvl (S.empty, taskManagerWithTask)

    it "hasTask returns true only for the added task"
      pending

    it "gets assigned to a dwarf" $ do
      let dwarf = findDwarf lvl
      taskManagerAssigned `shouldSatisfy` isAssignedTo task dwarf

    it "gets assigned to a dwarf and executed" $ do
      e <- runErrorT $ gameStepShouldChangeLevelTo [ "## "
                                                   , "@# "
                                                   , "   "
                                                   ]
                       >=>
                       gameStepShouldChangeLevelTo [ "#@ "
                                                   , " # "
                                                   , "   "
                                                   ]
                       >=>
                       gameStepShouldChangeLevelTo [ "#@ "
                                                   , " # "
                                                   , "   "
                                                   ]
                     $ (lvl, cmdSchedAssigned, taskManagerAssigned)
      case e of
        Left e' -> expectationFailure $ show e'
        Right _ -> return ()
