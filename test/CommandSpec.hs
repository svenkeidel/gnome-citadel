{-# LANGUAGE ScopedTypeVariables #-}
module CommandSpec(main, spec) where

import Control.Lens((^.), use)
import Control.Monad.State
import Control.Monad.Error

import Test.Hspec
import TestHelper

import Level
import Level.Command
import Level.Scheduler hiding (level)

import qualified Level.Scheduler as CS

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "An Execution" $ do

  let level = createLevel $
              unlines [ "## "
                      , " # "
                      , "@  "
                      ]
      dwarf' = level ^. findDwarf

      levelShouldBe s = do
        lvl <- use CS.level
        lift $ lift $ show lvl `shouldBe` s

      gameStepShouldChangeLevelTo s = executeGameStep >> levelShouldBe (unlines s)

  describe "An Approach" $ do
    it "moves an actor in multiple game steps to a destination coordinate" $ do

      e <- runErrorT $ flip runCommandScheduler level $ do
        addCommandT $ approach dwarf' (from2d (2,0))
        gameStepShouldChangeLevelTo [ "## "
                                    , " # "
                                    , " @ "
                                    ]

        gameStepShouldChangeLevelTo [ "## "
                                    , " #@"
                                    , "   "
                                    ]

        gameStepShouldChangeLevelTo [ "##@"
                                    , " # "
                                    , "   "
                                    ]

        gameStepShouldChangeLevelTo [ "##@"
                                    , " # "
                                    , "   "
                                    ]

      e `shouldSatisfy` isRight

    it "should approach an adjacent field if the destination is blocked" $ do
      e <- runErrorT $ flip runCommandScheduler level $ do
        addCommandT $ approach dwarf' (from2d (1,0))
        gameStepShouldChangeLevelTo [ "## "
                                    , "@# "
                                    , "   "
                                    ]

        gameStepShouldChangeLevelTo [ "## "
                                    , "@# "
                                    , "   "
                                    ]

      e `shouldSatisfy` isRight

  describe "A Mining Command" $ do
    it "should approach the mining location, to remove the field and place the actor on top" $ do
      e <- runErrorT $ flip runCommandScheduler level $ do
        wall' <- use $ CS.level . findWall (1,0)
        addCommandT $ mine dwarf' wall'
        gameStepShouldChangeLevelTo [ "## "
                                    , "@# "
                                    , "   "
                                    ]

        gameStepShouldChangeLevelTo [ "#@ "
                                    , " # "
                                    , "   "
                                    ]

        gameStepShouldChangeLevelTo [ "#@ "
                                    , " # "
                                    , "   "
                                    ]

      e `shouldSatisfy` isRight
