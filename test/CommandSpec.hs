{-# LANGUAGE ScopedTypeVariables #-}
module CommandSpec(main, spec) where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error

import Test.Hspec

import Level
import Level.Commands
import Tile
import Renderable

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "An Execution" $ do

    let levelString = unlines
          [ "## "
          , " # "
          , "@  "
          ]
        levelBuilder char =
          case char of
              '#' -> Right wall
              '@' -> Left dwarf
              _   -> error ("unrecognized char " ++ show char)
        level = fromString levelBuilder levelString
        Just (dwarfId,_) = flip runReader level $ findActor (\a -> render (toTile a) == '@')
        
        shouldBe' f x = do
          s :: Level <- get
          lift $ lift $ f s `shouldBe` x

        isRight = either (const False) (const True)
        isLeft  = either (const True)  (const False)

    describe "A Move" $ do  
      it "moves an actor in one step to an adjacent coordinate" $ do
        
        pending

        e <- runErrorT $ flip execStateT level $ do
          addCommand $ move dwarfId (from2d (1,2))
          executeGameStep
          show `shouldBe'` (unlines
            [ "## "
            , " # "
            , " @ "
            ])
          addCommand $ move dwarfId (from2d (0,1))
          executeGameStep
          show `shouldBe'` (unlines
            [ "## "
            , "@# "
            , "   "
            ])
        e `shouldSatisfy` isRight

      it "cannot be executed if the destination is blocked" $ do

        pending

        e' <- runErrorT $ flip execStateT level $ do
          addCommand $ move dwarfId (from2d (1,1))
          executeGameStep
        e' `shouldSatisfy` isLeft

         
    describe "An Approach" $ do
      it "moves an actor in multiple game steps to a destination coordinate" $ do

        pending

        e <- runErrorT $ flip execStateT level $ do
          lvl <- get
          let Right exec = flip runReader lvl $ runErrorT $ approach dwarfId (from2d (2,0))
          addCommand exec
          executeGameStep
          show `shouldBe'` (unlines
            [ "## "
            , " # "
            , " @ "
            ])
          executeGameStep
          show `shouldBe'` (unlines
            [ "## "
            , " # "
            , "  @"
            ])
          executeGameStep
          show `shouldBe'` (unlines
            [ "## "
            , " #@"
            , "   "
            ])
          executeGameStep
          show `shouldBe'` (unlines
            [ "##@"
            , " # "
            , "   "
            ])
          executeGameStep
          show `shouldBe'` (unlines
            [ "##@"
            , " # "
            , "   "
            ])
        e `shouldSatisfy` isRight
