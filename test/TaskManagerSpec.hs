module TaskManagerSpec(main, spec) where

-- import Control.Lens(_1)
-- import Control.Monad.State
-- import Control.Monad.Error

-- import Level
-- import TaskManagement

-- import qualified Level.Task as LevelTask
-- import Level.Scheduler
-- import Level.Transformation

-- import TestHelper
import Test.Hspec

-- type TaskManagerState = (Level, CommandScheduler, TaskManager)
-- type TaskManagerStateE = ErrorT LevelError IO TaskManagerState

main :: IO ()
main = hspec spec

-- everything :: a -> b -> Bool
-- everything _ _ = True

spec :: Spec
spec = describe "The TaskManager" $ return ()
--   let lvl = createLevel $
--               unlines [ "## "
--                       , " # "
--                       , "@  "
--                       ]
--       tId = 1
--       task = LevelTask.mine $ findWall (1,0) lvl

--       -- taskManagerShouldSatisfy x tm =
--       --   lift $ tm `shouldSatisfy` x

--       executeGameStep' :: TaskManagerState -> TaskManagerStateE
--       executeGameStep' (lvl, sched, tm)= ErrorT . return $ fmap (\(a,b) -> (a,b,tm)) (executeGameStep (lvl,sched))

--       gameStepShouldChangeLevelTo :: [String] -> TaskManagerState -> TaskManagerStateE
--       gameStepShouldChangeLevelTo s =
--         executeGameStep' >=> mapLevel (levelShouldBe s)

--       mapLevel :: Functor m => (Level -> m Level) -> TaskManagerState -> m TaskManagerState
--       mapLevel = _1

  -- context "asking for a task" $ do
  --   it "should return nothing if the task cannot be found" $ do
  --     undefined

  -- context "when adding tasks" $ do
  --   it "hasTask returns true only for the added task" $ do
  --     undefined

    -- it "gets assigned to a dwarf and executed" $ do
    --   pending

    --   e <- runErrorT $ flip runTaskManager lvl $ do

    --     reachable .= everything

    --     addTask task

    --     assignTasks

    --     gameStepShouldChangeLevelTo [ "## "
    --                                 , "@# "
    --                                 , "   "
    --                                 ]

    --     gameStepShouldChangeLevelTo [ "#@ "
    --                                 , " # "
    --                                 , "   "
    --                                 ]


    --     gameStepShouldChangeLevelTo [ "#@ "
    --                                 , " # "
    --                                 , "   "
    --                                 ]

    --   e `shouldSatisfy` isRight
