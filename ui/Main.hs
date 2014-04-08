module Main where

import Graphics.Vty
import TestHelper
import Level
import Counter
import TaskManagement
import qualified Level.Task as LevelTask

type TaskManagerState = (Level, TaskManager)
type TaskManagerStateE = ([AbortedTask],Level,TaskManager)

main :: IO ()
main = do
  vty <- mkVty
  let lvlInit = createLevel $ unlines ["  ####  "
                                      ,"  ######"
                                      ,"        "
                                      ,"  m  c  "
                                      ]

  eventLoop vty (lvlInit,empty)
  shutdown vty

  where
    addTask' task (lvl, ts) = let taskManagerWithTask = addTask task ts
                                  ts' = assignTasks lvl taskManagerWithTask
                              in (lvl,ts')

    ignoreErrors (_,l,t) = (l,t)

    onKeyPressed _ c state@(lvl,tm) =
      case c of
           '.' -> return $ ignoreErrors $ executeGameStep lvl tm
           'm' -> return $ addTask' (LevelTask.mine (findWall (5,0) lvl) lvl (Identifier 1)) state
           _   -> return $ state

    eventLoop vty state@(lvl,_) = do
      update vty (pic_for_image $ vert_cat $ map (string def_attr) $ lines $ show lvl)
      e <- next_event vty
      case e of
        EvKey k _ ->
          case k of
            (KASCII 'q') -> return ()
            (KASCII c)   -> do
              state' <- onKeyPressed vty c state
              eventLoop vty state'
            _ -> eventLoop vty state
        _ -> eventLoop vty state
