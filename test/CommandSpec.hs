{-# LANGUAGE ScopedTypeVariables #-}
module CommandSpec(main, spec) where

import Control.Lens (_1,_2)
import Control.Lens.Operators
import Control.Monad.State
import Control.Monad.Error

import Test.Hspec
import TestHelper

import Level
import Level.Transformation
import Level.Command
import qualified Level.Command as LC
import qualified Level.Scheduler as CS

type SchedulerState = (Level,CS.CommandScheduler)
type SchedulerStateE = ErrorT LevelError IO SchedulerState

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "An Execution" $ do

  let level = createLevel $
              unlines [ "## "
                      , " # "
                      , "@  "
                      ]
      dwarf' = findDwarf level

      executeGameStep' :: SchedulerState -> SchedulerStateE
      executeGameStep' = ErrorT . return . CS.executeGameStep

      gameStepShouldChangeLevelTo :: [String] -> SchedulerState -> SchedulerStateE
      gameStepShouldChangeLevelTo expected =
        executeGameStep' >=> mapLevel (levelShouldBe expected)

      mapLevel :: Functor m => (Level -> m Level) -> SchedulerState -> m SchedulerState
      mapLevel = _1

      addCommand' :: (Level -> Command) -> SchedulerState -> SchedulerStateE
      addCommand' f s@(lvl,_) = return $ s & _2 %~ CS.addCommand (f lvl)

  describe "An Approach" $ do
    it "moves an actor in multiple game steps to a destination coordinate" $ do

      e <- runErrorT $
        addCommand' (approach dwarf' (from2d (2,0)))
        >=>
        gameStepShouldChangeLevelTo [ "## "
                                    , " # "
                                    , " @ "
                                    ]
        >=>
        gameStepShouldChangeLevelTo [ "## "
                                    , " #@"
                                    , "   "
                                    ]
        >=>
        gameStepShouldChangeLevelTo [ "##@"
                                    , " # "
                                    , "   "
                                    ]
        >=>
        gameStepShouldChangeLevelTo [ "##@"
                                    , " # "
                                    , "   "
                                    ]
         $ (level, CS.empty)
      e `shouldSatisfy` isRight

    it "should approach an adjacent field if the destination is blocked" $ do
      e <- runErrorT $
        addCommand' (approach dwarf' (from2d (1,0))) >=>
        gameStepShouldChangeLevelTo [ "## "
                                    , "@# "
                                    , "   "
                                    ]
        >=>
        gameStepShouldChangeLevelTo [ "## "
                                    , "@# "
                                    , "   "
                                    ]
         $ (level, CS.empty)

      e `shouldSatisfy` isRight

  describe "A Mining Command" $
    it "should approach the mining location, to remove the field and place the actor on top" $ do
      let wall = findWall (1,0) level
      e <- runErrorT $
        addCommand' (LC.mine wall dwarf') >=>
        gameStepShouldChangeLevelTo [ "## "
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
         $ (level, CS.empty)
      e `shouldSatisfy` isRight
