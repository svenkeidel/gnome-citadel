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
import HspecHelper
import Test.Hspec

type TaskManagerState = (Level, S.CommandScheduler, TaskManager)
type TaskManagerStateE = ErrorT LevelError IO TaskManagerState

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "The TaskManager" $ do
  let
      executeGameStep' :: TaskManagerState -> TaskManagerStateE
      executeGameStep' (lvl', sched, tm) = ErrorT . return $ fmap (\(a,b) -> (a,b,tm)) (S.executeGameStep (lvl',sched))

      gameStepShouldChangeLevelTo :: [String] -> TaskManagerState -> TaskManagerStateE
      gameStepShouldChangeLevelTo s =
        executeGameStep' >=> mapLevel (levelShouldBe s)

      mapLevel :: Functor m => (Level -> m Level) -> TaskManagerState -> m TaskManagerState
      mapLevel = _1

      bootstrap lvl' task' = let taskManagerWithTask = addTask task' empty
                             in assignTasks lvl' (S.empty, taskManagerWithTask)

  context "when adding tasks" $ do

    it "cannot be assigned to a incompatible dwarf" $ do
      let lvl = createLevel $ unlines [ "## "
                                      , " # "
                                      , "c  "
                                      ]
          (_, taskManagerAssigned) = bootstrap lvl task
          task = LevelTask.mine (findWall (1,0) lvl) lvl (Identifier 1)
          dwarf = findDwarf 'c' lvl
      taskManagerAssigned `shouldSatisfy` not . isAssignedTo task dwarf

    it "cannot be assigned if dwarf cannot reach task location" $ do
      let lvl = createLevel $ unlines [ " # "
                                      , "   "
                                      , "###"
                                      , "m  "
                                      ]
          (_, taskManagerAssigned) = bootstrap lvl task
          task = LevelTask.mine (findWall (1,0) lvl) lvl (Identifier 1)
          dwarf = findDwarf 'm' lvl
      taskManagerAssigned `shouldSatisfy` not . isAssignedTo task dwarf

    it "gets assigned to the nearest dwarf" $ do
      let lvl = createLevel $ unlines [ " # "
                                      , "  m"
                                      , "   "
                                      , "m  "
                                      ]
          (_, taskManagerAssigned) = bootstrap lvl task
          task = LevelTask.mine (findWall (1,0) lvl) lvl (Identifier 1)
          dwarfLowerLeft = findDwarfByCoord (from2d (0,3)) lvl
          dwarfUpperRight = findDwarfByCoord (from2d (2,1)) lvl
      taskManagerAssigned `shouldSatisfy` isAssignedTo task dwarfUpperRight
      taskManagerAssigned `shouldSatisfy` not . isAssignedTo task dwarfLowerLeft

    it "gets assigned to a dwarf" $ do
      let lvl = createLevel $ unlines [ "## "
                                      , " # "
                                      , "m  "
                                      ]
          task = LevelTask.mine (findWall (1,0) lvl) lvl (Identifier 1)
          (_, taskManagerAssigned) = bootstrap lvl task
          dwarf = findDwarf 'm' lvl
      taskManagerAssigned `shouldSatisfy` isAssignedTo task dwarf

    it "gets assigned to a dwarf and executed" $ do
      let lvl = createLevel $ unlines [ "## "
                                      , " # "
                                      , "m  "
                                      ]
          task = LevelTask.mine (findWall (1,0) lvl) lvl (Identifier 1)
          (cmdSchedAssigned, taskManagerAssigned) = bootstrap lvl task
      e <- runErrorT $ gameStepShouldChangeLevelTo [ "## "
                                                   , "m# "
                                                   , "   "
                                                   ]
                       >=>
                       gameStepShouldChangeLevelTo [ "#m "
                                                   , " # "
                                                   , "   "
                                                   ]
                       >=>
                       gameStepShouldChangeLevelTo [ "#m "
                                                   , " # "
                                                   , "   "
                                                   ]
                     $ (lvl, cmdSchedAssigned, taskManagerAssigned)
      case e of
        Left e' -> expectationFailure $ show e'
        Right _ -> return ()
