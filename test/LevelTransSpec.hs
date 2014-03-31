{-# LANGUAGE ScopedTypeVariables #-}
module LevelTransSpec(main, spec) where

import Control.Monad.State
import Control.Monad.Error

import Test.Hspec

import Level
import Level.Transformation
import TestHelper
import HspecHelper

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "A Level Transformation" $ do
  let level = createLevel $
              unlines [ "## "
                      , " # "
                      , "m  "
                      ]
      dwarf' = findDwarf 'm' level
      wall'  = findWall (1, 0) level
      move' coord = ErrorT . return . move dwarf' (from2d coord)
      mine' coord = ErrorT . return . mine dwarf' (findWall coord level)
      failOnMissingItem' coord = ErrorT . return . failOnMissingItem dwarf' wall' (from2d coord)

  describe "A Move" $ do

    it "moves an actor in one step to an adjacent coordinate" $ do
      e <- runErrorT
            $  move' (1,2)
           >=> levelShouldBe [ "## "
                             , " # "
                             , " m "
                             ]
           >=> move' (0,1)
           >=> levelShouldBe [ "## "
                             , "m# "
                             , "   "
                             ]
            $ level
      e `shouldSatisfy` isRight

    it "cannot be executed if the destination is blocked" $ do
      e <- runErrorT $ move' (1,1) level
      e `shouldSatisfy` isLeft

  describe "Mining" $
    it "removes the mining target and places the actor on the empty field" $ do
      e <- runErrorT
            $  mine' (1,1)
           >=> levelShouldBe [ "## "
                             , " m "
                             , "   "
                             ]

           >=> mine' (0,0)
           >=> levelShouldBe [ "m# "
                             , "   "
                             , "   "
                             ]
            $ level
      e `shouldSatisfy` isRight

  describe "Fail on item missing" $ do
    it "fails if the item is not on the specified location" $ do
      let e = failOnMissingItem dwarf' wall' (from2d (2,2)) level
      e `shouldSatisfy` isLeft

    it "works with alternative" $ do
      e <- runErrorT $ failOnMissingItem' (1, 0) level
      e `shouldSatisfy` isRight
