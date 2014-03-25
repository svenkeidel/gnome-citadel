module TaskManagerSpec(main, spec) where

import Control.Lens.Operators
-- import Control.Monad.State
import Control.Monad.Error

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
      task = LevelTask.mine (findWall (1,0) lvl) lvl

      executeGameStep' :: TaskManagerState -> TaskManagerStateE
      executeGameStep' (lvl', sched, tm) = ErrorT . return $ fmap (\(a,b) -> (a,b,tm)) (S.executeGameStep (lvl',sched))

      gameStepShouldChangeLevelTo :: [String] -> TaskManagerState -> TaskManagerStateE
      gameStepShouldChangeLevelTo s =
        executeGameStep' >=> mapLevel (levelShouldBe s)

      mapLevel :: Functor m => (Level -> m Level) -> TaskManagerState -> m TaskManagerState
      mapLevel f (l, c, t) = fmap (\l' -> (l', c, t)) (f l)

  context "asking for a task" $
    it "should return nothing if the task cannot be found"
      pending

  context "when adding tasks" $ do
    it "hasTask returns true only for the added task"
      pending

    it "gets assigned to a dwarf" $ do
      let tm = taskManager & addTask task
          cmdSched = S.empty
          (_, tm') = assignTasks lvl (cmdSched, tm)
          dwarf = findDwarf lvl
      isAssignedTo (task 1) dwarf tm' `shouldBe` True

    it "gets assigned to a dwarf and executed" $ do
      let tm = taskManager & addTask task
          cmdSched = S.empty
          (cmdSched', tm') = assignTasks lvl (cmdSched, tm)

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
                     $ (lvl, cmdSched', tm')
      case e of
        Left e' -> expectationFailure $ show e'
        Right _ -> return ()
