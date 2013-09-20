{-# LANGUAGE ScopedTypeVariables #-}
module CommandSpec(main, spec) where

import Control.Lens((^.), (.~), (&), use)
import Control.Monad.State
import Control.Monad.Error

import Data.List(find)
import Data.Maybe(isJust)

import Test.Hspec

import Level
import Level.Command
import Level.Scheduler hiding (level)
import Tile
import Renderable
import TestTiles

import qualified Level.Scheduler as CS

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

      findWall c = do
        lvl <- use CS.level
        return $ head
               $ lvl ^. findStaticElement (\t -> render (toTile t) == '#'
                                              && lvl ^. coordOf t  == from2d c)

      levelShouldBe s = do
        lvl <- use CS.level
        lift $ lift $ show lvl `shouldBe` s

      gameStepShouldChangeLevelTo s = executeGameStep >> levelShouldBe (unlines s)

      isRight (Right _) = True
      isRight _         = False
      --isLeft = not . isRight

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
        wall' <- findWall (1,0)
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
