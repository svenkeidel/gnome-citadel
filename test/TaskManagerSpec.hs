module TaskManagerSpec(main, spec) where

import           Control.Lens (_1)
import           Control.Lens.At (contains)
import           Control.Lens.Operators
import           Control.Monad.Error
import           Control.Monad.Writer

import           Counter
import           Level
import qualified Level.Task as LevelTask
import           Task hiding (unless)
import           TaskManagement

import           HspecHelper
import           Test.Hspec
import           TestHelper

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

  it "can deduce if the given task is reachable by a dwarf" $ do
    let lvl = createLevel $ unlines [ "  #  "
                                    , " ##  "
                                    , "m #  "
                                    ]
        Just task = LevelTask.mine (findWall lvl (1,1)) lvl (Identifier 1)
        tm = addTask task empty
        dwarf = findDwarf lvl 'm'
    reachable (calculateReachable tm lvl) task dwarf `shouldBe` Reachable

  it "can deduce if the given task is not reachable by a dwarf" $ do
    let lvl = createLevel $ unlines [ "  #  "
                                    , "  ## "
                                    , "m #  "
                                    ]
        Just task = LevelTask.mine (findWall lvl (3,1)) lvl (Identifier 1)
        tm = addTask task empty
        dwarf = findDwarf lvl 'm'
    reachable (calculateReachable tm lvl) task dwarf `shouldBe` Unreachable

  context "when adding tasks" $ do

    it "cannot be assigned to a incompatible dwarf" $ do
      let lvl = createLevel $ unlines [ "## "
                                      , " # "
                                      , "c  "
                                      ]
          taskManagerAssigned = assignTask' lvl task
          Just task = LevelTask.mine (findWall lvl (1,0)) lvl (Identifier 1)
          dwarf = findDwarf lvl 'c'
      taskManagerAssigned `shouldSatisfy` not . isAssignedTo task dwarf

    it "cannot be assigned if dwarf cannot reach task location" $ do
      let lvl = createLevel $ unlines [ " # "
                                      , "   "
                                      , "###"
                                      , "m  "
                                      ]
          taskManagerAssigned = assignTask' lvl task
          Just task = LevelTask.mine (findWall lvl (1,0)) lvl (Identifier 1)
          dwarf = findDwarf lvl 'm'
      taskManagerAssigned `shouldSatisfy` not . isAssignedTo task dwarf

    it "gets assigned to the nearest dwarf" $ do
      let lvl = createLevel $ unlines [ " # "
                                      , "  m"
                                      , "   "
                                      , "m  "
                                      ]
          taskManagerAssigned = assignTask' lvl task
          Just task = LevelTask.mine (findWall lvl (1,0)) lvl (Identifier 1)
          dwarfLowerLeft = findDwarfByCoord lvl (from2d (0,3))
          dwarfUpperRight = findDwarfByCoord lvl (from2d (2,1))
      taskManagerAssigned `shouldSatisfy` isAssignedTo task dwarfUpperRight
      taskManagerAssigned `shouldSatisfy` not . isAssignedTo task dwarfLowerLeft

    it "gets assigned to a dwarf" $ do
      let lvl = createLevel $ unlines [ "## "
                                      , " # "
                                      , "m  "
                                      ]
          Just task = LevelTask.mine (findWall lvl (1,0)) lvl (Identifier 1)
          taskManagerAssigned = assignTask' lvl task
          dwarf = findDwarf lvl 'm'
      taskManagerAssigned `shouldSatisfy` isAssignedTo task dwarf

    it "gets not assigned if suitable tool not available" $ do
      pending
      let lvl = createLevel $ unlines [ "## "
                                      , " # "
                                      , "m  "
                                      ]
          Just task = LevelTask.mine (findWall lvl (1,0)) lvl (Identifier 1)
          taskManagerAssigned = assignTask' lvl task
          dwarf = findDwarf lvl 'm'
      taskManagerAssigned `shouldSatisfy` not . isAssignedTo task dwarf

    it "gets assigned to a dwarf and executed" $ do
      let lvl = createLevelWithTools $ unlines [ "## "
                                               , " # "
                                               , "m  "
                                               ]
          Just task = LevelTask.mine (findWall lvl (1,0)) lvl (Identifier 1)
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

      unless (null e) $ expectationFailure $ show e

    it "is marked as completed after successful execution" $ do
      let lvl = createLevelWithTools $ unlines [ "## "
                                               , " # "
                                               , "m  "
                                               ]
          Just task = LevelTask.mine (findWall lvl (1,0)) lvl (Identifier 1)
          taskManagerAssigned = assignTask' lvl task
          dwarf = findDwarf lvl 'm'

      ((_,tm'),e) <- runWriterT $
        foldr1 (>=>) (replicate 10 executeGameStep') (lvl,taskManagerAssigned)

      unless (null e) $ expectationFailure $ show e

      tm' ^. inactive . contains task `shouldBe` False

      elem task (getTask (tm' ^. active)) `shouldBe` False

      tm' `shouldSatisfy` not . isAssignedTo task dwarf

    it "walls are mined" $ do
      let lvl = createLevelWithTools $ unlines ["  ####  "
                                               ,"  ##### "
                                               ,"        "
                                               ,"  m  m  "
                                               ]
          Just task1 = LevelTask.mine (findWall lvl (2,0)) lvl (Identifier 1)
          Just task2 = LevelTask.mine (findWall lvl (5,0)) lvl (Identifier 2)
          taskManagerAssigned = assignTasks lvl $ addTask task2 $ addTask task1 empty

      ((_,tm'),e) <- runWriterT $
        foldr1 (>=>) (replicate 10 executeGameStep') (lvl,taskManagerAssigned)

      unless (null e) $ expectationFailure $ show e

      tm' `shouldBe` empty

    it "doesn't conflict if two dwarfs race for the pickaxe" $ do
      let lvl = createLevel $ unlines ["  ####  "
                                      ,"  ##### "
                                      ,"        "
                                      ,"m   x  m"
                                      ]
          Just task1 = LevelTask.mine (findWall lvl (2,0)) lvl (Identifier 1)
          Just task2 = LevelTask.mine (findWall lvl (5,0)) lvl (Identifier 2)
          taskManagerAssigned = assignTasks lvl $ addTask task2 $ addTask task1 empty

      _ <- execWriterT $ gameStepShouldChangeLevelTo ["  ####  "
                                                     ,"  ##### "
                                                     ,"        "
                                                     ," m  x m "
                                                     ]
                         >=>
                         gameStepShouldChangeLevelTo ["  ####  "
                                                     ,"  ##### "
                                                     ,"        "
                                                     ,"  m xm  "
                                                     ]
                         >=>
                         gameStepShouldChangeLevelTo ["  ####  "
                                                     ,"  ##### "
                                                     ,"        "
                                                     ,"   mm   "
                                                     ]
                         >=>
                         gameStepShouldChangeLevelTo ["  ####  "
                                                     ,"  ##### "
                                                     ,"        "
                                                     ,"    m   "
                                                     ]
                         >=>
                         gameStepShouldChangeLevelTo ["  ####  "
                                                     ,"  ##### "
                                                     ,"    m   "
                                                     ,"    m   "
                                                     ]
                       $ (lvl, taskManagerAssigned)

      return ()

    it "two dwarf race for the same pickaxe" $ do
      let levelWithPickAxe = createLevel $ unlines [ "# m"
                                                   , " x "
                                                   , "m #"
                                                   ]
          wall1 = findWall levelWithPickAxe (0,0)
          wall2 = findWall levelWithPickAxe (2,2)
          Just task1 = LevelTask.mine wall1 levelWithPickAxe (Identifier 1)
          Just task2 = LevelTask.mine wall2 levelWithPickAxe (Identifier 2)
          taskManagerAssigned = assignTasks levelWithPickAxe $ addTask task2 $ addTask task1 empty
      (_,es) <- runWriterT $
        gameStepShouldChangeLevelTo [ "#  "
                                    , " m "
                                    , "  #"
                                    ]
        >=>
        gameStepShouldChangeLevelTo [ "m  "
                                    , "   "
                                    , "  m"
                                    ]
         $ (levelWithPickAxe, taskManagerAssigned)
      es `shouldSatisfy` null -- no errors should have occured

    where
      getTask = map (\(ActiveTask t _) -> t)
