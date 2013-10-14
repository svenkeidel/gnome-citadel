{-# LANGUAGE ScopedTypeVariables #-}
module CommandSpec(main, spec) where

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

      executeGameStep' :: SchedulerState -> ErrorT LevelError IO SchedulerState
      executeGameStep' = ErrorT . return . CS.executeGameStep

      gameStepShouldChangeLevelTo :: [String] -> SchedulerState -> ErrorT LevelError IO SchedulerState
      gameStepShouldChangeLevelTo expected =
        executeGameStep' >=> mapLevel (levelShouldBe expected)

      mapLevel :: Monad m => (Level -> m Level) -> SchedulerState -> m SchedulerState
      mapLevel f (lvl, cmdSched) = do
        lvl' <- f lvl
        return $ (lvl', cmdSched)

      addCommandT' :: Monad m => (Level -> CommandT m) -> SchedulerState -> m SchedulerState
      addCommandT' f (lvl,cmdSched) = do
        cmdSched' <- CS.addCommandT (f lvl) cmdSched
        return (lvl, cmdSched')

  describe "An Approach" $ do
    it "moves an actor in multiple game steps to a destination coordinate" $ do

      e <- runErrorT $
        addCommandT' (approach dwarf' (from2d (2,0)))
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
        addCommandT' (approach dwarf' (from2d (1,0))) >=>
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

  describe "A Mining Command" $ do
    it "should approach the mining location, to remove the field and place the actor on top" $ do
      let wall = findWall (1,0) level
      e <- runErrorT $
        addCommandT' (LC.mine wall dwarf') >=>
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
