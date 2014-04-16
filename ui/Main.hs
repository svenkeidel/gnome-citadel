{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens.TH
import Control.Lens.Operators
import Control.Lens (_1, _2)

import Graphics.Vty
import TestHelper
import Level
import Counter
import TaskManagement
import qualified Level.Task as LevelTask

data GameState = GameState { _cursor :: (Int,Int), _level:: Level, _taskManager :: TaskManager }
makeLenses ''GameState

main :: IO ()
main = do
  vty <- mkVty
  let lvlInit = createLevel $ unlines ["  ####  "
                                      ,"  ##### "
                                      ,"        "
                                      ,"  m  c  "
                                      ]

  eventLoop vty (GameState (0,0) lvlInit empty)
  shutdown vty

  where
    addTask' task (GameState csr lvl ts) = let taskManagerWithTask = addTask task ts
                                               ts' = assignTasks lvl taskManagerWithTask
                                           in GameState csr lvl ts'

    ignoreErrors csr (_,l,t) = GameState csr l t

    onKeyPressed _ c state@(GameState csr lvl tm) =
      case c of
           '.' -> return $ ignoreErrors csr $ executeGameStep lvl tm
           'm' -> return $ addTask' (LevelTask.mine (findWall csr lvl) lvl (Identifier 1)) state
           'h' -> return $ moveCursor state (-1) 0
           'j' -> return $ moveCursor state  0   1
           'k' -> return $ moveCursor state  0 (-1)
           'l' -> return $ moveCursor state  1   0
           _   -> return state

    moveCursor (GameState (x,y) lvl t) dx dy = GameState (x',y') lvl t
       where
           x' = min (max 0 (x + dx)) (lvl ^. bounds . _1)
           y' = min (max 0 (y + dy)) (lvl ^. bounds . _2)

    eventLoop vty state@(GameState (x,y) lvl _) = do
      update vty ((pic_for_image $ vert_cat $ map (string def_attr) $ lines $ show lvl)
                      { pic_cursor = Cursor (fromIntegral x) (fromIntegral y)})
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
