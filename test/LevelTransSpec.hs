{-# LANGUAGE ScopedTypeVariables #-}
module LevelTransSpec(main, spec) where

import Control.Monad.State
import Control.Monad.Error

import Test.Hspec

import Utils (fromRight)
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
      dwarf' = findDwarf level

  describe "A Move" $ do

    it "moves an actor in one step to an adjacent coordinate" $ do
      let mover = fromRight . move dwarf' (from2d (1,2))
      show (mover level) `shouldBe` unlines [ "## "
                                            , " # "
                                            , " @ "
                                            ]
      let mover' = fromRight . move dwarf' (from2d (0,1))
      show (mover' level) `shouldBe` unlines [ "## "
                                             , "@# "
                                             , "   "
                                             ]

    it "cannot be executed if the destination is blocked" $ do
      let result = move dwarf' (from2d (1,1)) level
      result `shouldSatisfy` isLeft

  describe "Mining" $ do
    it "removes the mining target and places the actor on the empty field" $ do
      let wall = findWall (1,1) level
          wall' = findWall (0,0) level
          mine' w = ErrorT . return . mine dwarf' w
      e <- runErrorT $
        -- ErrorT :: m (Either a b) -> ErrorT a m b
        mine' wall
        >=>
        levelShouldBe [ "## "
                       , " @ "
                       , "   "
                       ]
        >=>
        mine' wall'
        >=>
        levelShouldBe [ "@# "
                       , "   "
                       , "   "
                       ]
         $ level

      e `shouldSatisfy` isRight

  describe "Fail on item missing" $ do
    let wall = findWall (1,1) level

    it "fails if the item is not on the specified location" $ do
      let e = failOnMissingItem dwarf' wall (from2d (2,2)) level
      e `shouldSatisfy` isLeft

    it "works with alternative" $ do
      let move' = ErrorT . return . move dwarf' (from2d (1,2))
      e <- runErrorT $
        ErrorT . return . failOnMissingItem dwarf' wall (from2d (1,1))
        >=>
        move'
        >=>
        levelShouldBe [ "## "
                       , " # "
                       , " @ "
                       ]
         $ level

      e `shouldSatisfy` isRight
