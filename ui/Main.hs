module Main where

import Graphics.Vty
import TestHelper
import Level
import Counter
import qualified Level.Scheduler as S
import Level.Transformation
import TaskManagement
import qualified Level.Task as LevelTask

type TaskManagerState = (Level, S.CommandScheduler, TaskManager)
type TaskManagerStateE = Either LevelError TaskManagerState

main :: IO ()
main = do
  vty <- mkVty
  let lvlInit = createLevel $ unlines ["  ####  "
                                      ,"  ####  "
                                      ,"        "
                                      ,"  m  c  "
                                      ]

  eventLoop vty (lvlInit,S.empty,empty)
  shutdown vty

  where
    addTask' task (lvl, cs, ts) = let taskManagerWithTask = addTask task ts
                                      (cs',ts') = assignTasks lvl (cs, taskManagerWithTask)
                                  in (lvl,cs',ts')

    onKeyPressed _ c state@(lvl,_,_) =
      case c of
           '.' -> return $ executeGameStep' state
           'm' -> return $ Right $ addTask' (LevelTask.mine (findWall (5,0) lvl) lvl (Identifier 1)) state
           _   -> return $ Right state

    executeGameStep' :: TaskManagerState -> TaskManagerStateE
    executeGameStep' (lvl', sched, tm) = fmap (\(a,b) -> (a,b,tm)) (S.executeGameStep (lvl',sched))

    eventLoop vty state@(lvl,_,_) = do
      update vty (pic_for_image $ vert_cat $ map (string def_attr) $ lines $ show lvl)
      e <- next_event vty
      case e of
        EvKey k _ ->
          case k of
            (KASCII 'q') -> return ()
            (KASCII c)   -> do
              state' <- onKeyPressed vty c state
              case state' of
                Left _ -> undefined
                Right s -> eventLoop vty s
            _            -> eventLoop vty state
        _         -> eventLoop vty state
