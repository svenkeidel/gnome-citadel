{-# LANGUAGE ScopedTypeVariables #-}
module LevelTransSpec(main, spec) where

import Control.Lens((^.), use)
import Control.Monad.State
import Control.Monad.Error

import Test.Hspec

import Level
import Level.Transformation
import TestHelper

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "A Level Transformation" $ do
  let level = createLevel $
              unlines [ "## "
                      , " # "
                      , "@  "
                      ]
      dwarf' = level ^. findDwarf

      levelShouldBe s = do
        lvl <- get
        lift $ lift $ show lvl `shouldBe` unlines s

  describe "A Move" $ do

    it "moves an actor in one step to an adjacent coordinate" $ do
      e <- runErrorT $ flip execStateT level $ do
        move dwarf' (from2d (1,2))
        levelShouldBe [ "## "
                      , " # "
                      , " @ "
                      ]
        move dwarf' (from2d (0,1))
        levelShouldBe [ "## "
                      , "@# "
                      , "   "
                      ]

      e `shouldSatisfy` isRight

    it "cannot be executed if the destination is blocked" $ do
      e <- runErrorT $ flip runStateT level $ move dwarf' (from2d (1,1))
      e `shouldSatisfy` isLeft


  describe "Mining" $ do

    it "removes the mining target and places the actor on the empty field" $ do
      e <- runErrorT $ flip execStateT level $ do
        mine dwarf' =<< use (findWall (1,1)) :: LevelTrans IO
        levelShouldBe [ "## "
                      , " @ "
                      , "   "
                      ]

        mine dwarf' =<< use (findWall (0,0)) :: LevelTrans IO
        levelShouldBe [ "@# "
                      , "   "
                      , "   "
                      ]

      e `shouldSatisfy` isRight

  describe "Fail on item missing" $ do
    it "fails if the item is not on the specified location" $ do
      e <- runErrorT $ flip execStateT level $ do
        wall' <- use $ findWall (1,1)
        failOnMissingItem dwarf' wall' (from2d (2,2))

      e `shouldSatisfy` isLeft

    it "works with alternative" $ do
      e <- runErrorT $ flip execStateT level $ do
        wall' <- use $ findWall (1,1)
        failOnMissingItem dwarf' wall' (from2d (1,1)) >> move dwarf' (from2d (1,2))
        levelShouldBe [ "## "
                      , " # "
                      , " @ "
                      ]

      e `shouldSatisfy` isRight
