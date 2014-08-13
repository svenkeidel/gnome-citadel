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

data GameState = GameState { _cursor :: (Int,Int)
                           , _level:: Level
                           , _taskManager :: TaskManager
                           , _messages :: [String]
                           }
makeLenses ''GameState

main :: IO ()
main = do
  vty <- mkVty
  let lvlInit = createLevel $ unlines ["  ####  "
                                      ,"  ##### "
                                      ,"        "
                                      ,"  m  c  "
                                      ]

  eventLoop vty (GameState (0,0) lvlInit empty ["Game started."])
  shutdown vty

  where
    addTask' task (GameState csr lvl ts msgs) = let taskManagerWithTask = addTask task ts
                                                    ts' = assignTasks lvl taskManagerWithTask
                                                in GameState csr lvl ts' msgs


    logMessages (GameState csr _ _ msgs) (abortedTasks, lvl, tm) = GameState csr lvl tm (abortedTaskMessages abortedTasks ++ msgs)

    abortedTaskMessages :: [AbortedTask] -> [String]
    abortedTaskMessages = map (\ (AbortedTask _ msg) -> msg)

    onKeyPressed _ c state@(GameState csr lvl tm _) =
      case c of
           '.' -> return $ logMessages state $ executeGameStep lvl tm
           'm' -> return $ addTask' (LevelTask.mine (findWall csr lvl) lvl (Identifier 1)) state
           'h' -> return $ moveCursor state (-1) 0
           'j' -> return $ moveCursor state  0   1
           'k' -> return $ moveCursor state  0 (-1)
           'l' -> return $ moveCursor state  1   0
           _   -> return state

    moveCursor (GameState (x,y) lvl t msgs) dx dy = GameState (x',y') lvl t msgs
       where
           x' = min (max 0 (x + dx)) (lvl ^. bounds . _1)
           y' = min (max 0 (y + dy)) (lvl ^. bounds . _2)

    eventLoop vty state = do
      update vty (drawGame state)
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

drawGame :: GameState -> Picture
drawGame s@(GameState (x,y) lvl _ _) =
   setCursor (x,y) $ pic_for_image (lvlPic <-> drawMessages s)
  where lvlPic = vert_cat . map (string def_attr) . lines . show $ lvl

drawMessages :: GameState -> Image
drawMessages state = vert_cat . map (string def_attr) $ state ^. messages

setCursor :: (Int, Int) -> Picture -> Picture
setCursor (x,y) pic = pic { pic_cursor = Cursor (fromIntegral x) (fromIntegral y)}
