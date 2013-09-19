{-# LANGUAGE ScopedTypeVariables #-}
module CommandSpec(main, spec) where

import Control.Lens((^.), (.~), (&), use)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error

import Data.List(find)
import Data.Maybe(isJust)

import Test.Hspec

import Level
import Level.Commands
import Level.CommandScheduler hiding (level)
import qualified Level.CommandScheduler as CS
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
              '#' -> Just $ Right wall
              ' ' -> Just $ Right free
              '@' -> Just $ Left dwarf
              _   -> error ("unrecognized char " ++ show char)
        contains x = isJust . find ((x ==) . render)
        level  = fromString levelBuilder levelString
               & walkable .~ (\lvl coord -> not $ contains '#' (lvl ^. at coord))
        dwarf' = head $ level ^. findActor (\a -> render (toTile a) == '@')

        levelShouldBe s = do
          lvl <- use CS.level
          lift $ lift $ show lvl `shouldBe` s

        gameStepShouldChangeLevelTo s = executeGameStep >> levelShouldBe (unlines s)

        isRight (Right _) = True
        isRight _         = False
        isLeft = not . isRight

        approach' actor coord = do
          lvl <- use CS.level
          c <- runErrorT $ flip runReaderT lvl $ runCommandT $ approach actor coord
          addCommand $ case c of
            Left e'  -> error $ "Could not find path: " ++ show e'
            Right c' -> c'

    describe "A Move" $ do
      it "moves an actor in one step to an adjacent coordinate" $ do

        e <- runErrorT $ flip runCommandScheduler level $ do

             addCommand $ move dwarf' (from2d (1,2))
             gameStepShouldChangeLevelTo [ "## "
                                         , " # "
                                         , " @ "
                                         ]

             addCommand $ move dwarf' (from2d (0,1))
             gameStepShouldChangeLevelTo [ "## "
                                         , "@# "
                                         , "   "
                                         ]

        e `shouldSatisfy` isRight

      it "cannot be executed if the destination is blocked" $ do

        e' <- runErrorT $ flip runCommandScheduler level $ do
          addCommand $ move dwarf' (from2d (1,1))
          executeGameStep
        e' `shouldSatisfy` isLeft

    describe "An Approach" $ do
      it "moves an actor in multiple game steps to a destination coordinate" $ do

        e <- runErrorT $ flip runCommandScheduler level $ do
          approach' dwarf' (from2d (2,0))
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
          approach' dwarf' (from2d (1,0))
          gameStepShouldChangeLevelTo [ "## "
                                      , "@# "
                                      , "   "
                                      ]

          gameStepShouldChangeLevelTo [ "## "
                                      , "@# "
                                      , "   "
                                      ]

        e `shouldSatisfy` isRight
