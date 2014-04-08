module TaskManagerSpec(main, spec) where

import           Control.Lens.Operators
import           Control.Lens (_1)
import           Control.Lens.At (contains)
import           Control.Monad.Error
import           Control.Monad.Writer

import           Counter
import           Level
import           Task
import           TaskManagement
import qualified Level.Task as LevelTask

import           TestHelper
import           HspecHelper
import           Test.Hspec

type TaskManagerState = (Level, TaskManager)
type TaskManagerStateE = WriterT [AbortedTask] IO TaskManagerState

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "The TaskManager" $ do
  let
      executeGameStep' :: TaskManagerState -> TaskManagerStateE
      executeGameStep' (lvl, tm) =
        let (e,lvl',tm') = executeGameStep lvl tm
        in writer ((lvl',tm'), e)

      gameStepShouldChangeLevelTo :: [String] -> TaskManagerState -> TaskManagerStateE
      gameStepShouldChangeLevelTo s =
        executeGameStep' >=> mapLevel (levelShouldBe s)

      mapLevel :: Functor m => (Level -> m Level) -> TaskManagerState -> m TaskManagerState
      mapLevel = _1

      assignTask' lvl' task' = let taskManagerWithTask = addTask task' empty
                             in assignTasks lvl' taskManagerWithTask

  context "when adding tasks" $ do

    it "cannot be assigned to a incompatible dwarf" $ do
      let lvl = createLevel $ unlines [ "## "
                                      , " # "
                                      , "c  "
                                      ]
          taskManagerAssigned = assignTask' lvl task
          task = LevelTask.mine (findWall (1,0) lvl) lvl (Identifier 1)
          dwarf = findDwarf 'c' lvl
      taskManagerAssigned `shouldSatisfy` not . isAssignedTo task dwarf

    it "cannot be assigned if dwarf cannot reach task location" $ do
      let lvl = createLevel $ unlines [ " # "
                                      , "   "
                                      , "###"
                                      , "m  "
                                      ]
          taskManagerAssigned = assignTask' lvl task
          task = LevelTask.mine (findWall (1,0) lvl) lvl (Identifier 1)
          dwarf = findDwarf 'm' lvl
      taskManagerAssigned `shouldSatisfy` not . isAssignedTo task dwarf

    it "gets assigned to the nearest dwarf" $ do
      let lvl = createLevel $ unlines [ " # "
                                      , "  m"
                                      , "   "
                                      , "m  "
                                      ]
          taskManagerAssigned = assignTask' lvl task
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
          taskManagerAssigned = assignTask' lvl task
          dwarf = findDwarf 'm' lvl
      taskManagerAssigned `shouldSatisfy` isAssignedTo task dwarf

    it "gets assigned to a dwarf and executed" $ do
      let lvl = createLevel $ unlines [ "## "
                                      , " # "
                                      , "m  "
                                      ]
          task = LevelTask.mine (findWall (1,0) lvl) lvl (Identifier 1)
          taskManagerAssigned = assignTask' lvl task
      e <- execWriterT $ gameStepShouldChangeLevelTo [ "## "
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
                       $ (lvl, taskManagerAssigned)

      when (not $ null e) $ expectationFailure $ show e

    it "is marked as completed after successful execution" $ do
      let lvl = createLevel $ unlines [ "## "
                                      , " # "
                                      , "m  "
                                      ]
          task = LevelTask.mine (findWall (1,0) lvl) lvl (Identifier 1)
          taskManagerAssigned = assignTask' lvl task

      ((_,tm'),e) <- runWriterT $
        foldr1 (>=>) (replicate 10 executeGameStep') (lvl,taskManagerAssigned)

      when (not $ null e) $ expectationFailure $ show e

      tm' ^. inactive . contains task `shouldBe` False

      -- I couldn't do the lens version. If you can, please fix this.
      (elem task $ getTask (tm' ^. active)) `shouldBe` False

    where
      getTask = map (\(ActiveTask t _) -> t)
