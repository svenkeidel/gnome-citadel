{-# LANGUAGE ScopedTypeVariables #-}
module CommandSpec(main, spec) where

import Control.Lens (_1)
import Control.Monad.State
import Control.Monad.Error

import Test.Hspec
import TestHelper
import HspecHelper

import Level
import Level.Transformation
import Level.Command
import qualified Level.Command as LC
import Unfold

type SchedulerState = (Level,Command)
type SchedulerStateE = ErrorT LevelError IO SchedulerState

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "An Execution" $ do

  let level = createLevelWithTools $ unlines [ "## "
                                             , " # "
                                             , "m  "
                                             ]
      dwarf' = findDwarf level 'm'

      executeGameStep' :: SchedulerState -> SchedulerStateE
      executeGameStep' (lvl,com) =
        case next com of
          Done             -> return (lvl,com)
          Yield trans com' -> ErrorT $ return $ do
            lvl' <- trans lvl
            return (lvl',com')

      gameStepShouldChangeLevelTo :: [String] -> SchedulerState -> SchedulerStateE
      gameStepShouldChangeLevelTo expected =
        executeGameStep' >=> mapLevel (levelShouldBe expected)

      mapLevel :: Functor m => (Level -> m Level) -> SchedulerState -> m SchedulerState
      mapLevel = _1

      noError :: Either LevelError SchedulerState -> Expectation
      noError (Left l) = expectationFailure (show l)
      noError (Right _) = return ()

  describe "An Approach" $ do
    it "moves an actor in multiple game steps to a destination coordinate" $ do

      e <- runErrorT $
        gameStepShouldChangeLevelTo [ "## "
                                    , " # "
                                    , " m "
                                    ]
        >=>
        gameStepShouldChangeLevelTo [ "## "
                                    , " #m"
                                    , "   "
                                    ]
        >=>
        gameStepShouldChangeLevelTo [ "##m"
                                    , " # "
                                    , "   "
                                    ]
        >=>
        gameStepShouldChangeLevelTo [ "##m"
                                    , " # "
                                    , "   "
                                    ]
         $ (level, approach (from2d (2,0)) dwarf' level)

      noError e

    it "should approach an adjacent field if the destination is blocked" $ do
      e <- runErrorT $
        gameStepShouldChangeLevelTo [ "## "
                                    , "m# "
                                    , "   "
                                    ]
        >=>
        gameStepShouldChangeLevelTo [ "## "
                                    , "m# "
                                    , "   "
                                    ]
         $ (level, approach (from2d (1,0)) dwarf' level)

      noError e

  describe "A Mining Command" $
    it "should approach the mining location, to remove the field and place the actor on top" $ do
      let wall = findWall level (1,0)
      e <- runErrorT $
        gameStepShouldChangeLevelTo [ "## "
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
         $ (level, LC.mine wall dwarf' level)

      noError e
