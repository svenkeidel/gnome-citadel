{-# LANGUAGE ScopedTypeVariables #-}
module CommandSpec(main, spec) where

import Control.Coroutine
import Control.Coroutine.AwaitYield
import Control.Lens (_1)
import Control.Monad.State
import Control.Monad.Error

import Test.Hspec
import TestHelper
import HspecHelper

import Level
import Level.Transformation
import Level.Command
import qualified Level.Transformation as T
import qualified Level.Command as LC

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
      executeGameStep' (lvl,c) =
        case resume c of
          Left e                       -> throwError e
          Right (Right ())             -> return (lvl,c)
          Right (Left (Await f))       -> executeGameStep' (lvl,f lvl)
          Right (Left (Yield lvl' c')) -> return (lvl',c')

      gameStepShouldChangeLevelTo :: [String] -> SchedulerState -> SchedulerStateE
      gameStepShouldChangeLevelTo expected =
        executeGameStep' >=> mapLevel (levelShouldBe expected)

      mapLevel :: Functor m => (Level -> m Level) -> SchedulerState -> m SchedulerState
      mapLevel = _1

      noError :: Either LevelError SchedulerState -> Expectation
      noError (Left l) = expectationFailure (show l)
      noError (Right _) = return ()

  describe "A Level Transition" $ do
    it "can be run inside a command" $ do
      e <- runErrorT $
        gameStepShouldChangeLevelTo [ "## "
                                    , " # "
                                    , " m "
                                    ]
         $ (level, runTransformation (T.move dwarf' (from2d (1,2))))

      noError e

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
         $ (level, approach (from2d (2,0)) dwarf')

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
         $ (level, approach (from2d (1,0)) dwarf')

      noError e

  describe "A Mining Command" $ do
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
         $ (level, LC.mine wall dwarf')

      noError e

    it "needs the appropriate tool before the dwarf can begin to mine" $ do

      let levelWithPickAxe = createLevel $ unlines [ "##x"
                                                   , " # "
                                                   , "m  "
                                                   ]
          wall = findWall levelWithPickAxe (0,0)
          miningDwarf = findDwarf levelWithPickAxe 'm'

      e <- runErrorT $
        gameStepShouldChangeLevelTo [ "##x"
                                    , " # "
                                    , " m "
                                    ]
        >=>
        gameStepShouldChangeLevelTo [ "##x"
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
        >=>
        gameStepShouldChangeLevelTo [ "## "
                                    , " #m"
                                    , "   "
                                    ]
        >=>
        gameStepShouldChangeLevelTo [ "## "
                                    , " # "
                                    , " m "
                                    ]
        >=>
        gameStepShouldChangeLevelTo [ "## "
                                    , "m# "
                                    , "   "
                                    ]
        >=>
        gameStepShouldChangeLevelTo [ "m# "
                                    , " # "
                                    , "   "
                                    ]
         $ (levelWithPickAxe, LC.mine wall miningDwarf)

      noError e
