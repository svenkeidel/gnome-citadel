module Main where

import           Control.Monad.Error
import           Control.Monad.Writer
import           Criterion.Main
import           Criterion.Types
import           Data.Foldable (foldl')
import           Data.Maybe (catMaybes)

import           Control.Lens hiding (Level)

import           Counter
import           Level
import qualified Level.Task as LevelTask
import           Renderable
import           StaticElement
import           TaskManagement
import           TestHelper hiding (findWall)
import           Tile

type TaskManagerState = (Level, TaskManager)
type TaskManagerStateE = Writer [AbortedTask] TaskManagerState

main :: IO ()
main = defaultMainWith config bs
     where bs = [ bench "Gnome Citadel Level" $ nf runLevel' empty ]
           runLevel' :: TaskManager -> (TaskManagerState, [AbortedTask])
           runLevel' = runLevel 10 . mineAllWalls benchLevel

runLevel :: Int -> (Level, TaskManager) -> (TaskManagerState, [AbortedTask])
runLevel n (lvl,tm) = runWriter $
    foldr1 (>=>) (replicate n executeGameStep') (lvl,tm)

config :: Config
config = defaultConfig {
  reportFile= Just "dist/task-scheduling.html"
}

executeGameStep' :: TaskManagerState -> TaskManagerStateE
executeGameStep' (lvl, tm) =
  let (e,lvl',tm') = executeGameStep lvl tm
  in writer ((lvl',tm'), e)

findWall :: (Int, Int) -> Level -> Maybe StaticElement
findWall c lvl = preview _head . flip findStaticElement lvl $ \t ->
  render (toTile t) == '#' && lvl ^. coordOf t  == from2d c

findAllWalls :: Level -> [StaticElement]
findAllWalls lvl = catMaybes $ traverse findWall cs lvl
  where cs = [(cdx,cdy) | cdx <- [0..lvl^.bounds._1], cdy <- [0..lvl^.bounds._2]]

mineAllWalls :: Level -> TaskManager -> (Level,TaskManager)
mineAllWalls lvl tm = (lvl,fst $ foldl' (\(tm',i) w -> (addTask (LevelTask.mine w lvl i) tm',succ i)) (tm,Identifier 0) (findAllWalls lvl))

benchLevel :: Level
benchLevel = createLevel . unlines $
  [ "                                                           #####"
  , "                                                           #####"
  , "    #####                                                   m   "
  , "   #     # #    #  ####  #    # ######                          "
  , "   #       ##   # #    # ##  ## #                               "
  , "   #  #### # #  # #    # # ## # #####                           "
  , "   #     # #  # # #    # #    # #                    x          "
  , "   #     # #   ## #    # #    # #                               "
  , "    #####  #    #  ####  #    # ######                          "
  , "                                                                "
  , "                  ######                                        "
  , "                 #        # #####   ##   #####  ###### #        "
  , "                 #        #   #    #  #  #    # #      #        "
  , "                 #        #   #   #    # #    # #####  #        "
  , "                 #        #   #   ###### #    # #      #        "
  , "                 #        #   #   #    # #    # #      #        "
  , "   m              ######  #   #   #    # #####  ###### ######   "
  , "#####                                                           "
  , "#####                                                           "
  ]
