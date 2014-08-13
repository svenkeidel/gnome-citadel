{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Lens (_1, _2, preview, _head, folded, to)
import           Control.Lens.Operators
import           Control.Lens.TH
import           Graphics.Vty

import           Counter (Counter, Identifier)
import qualified Counter as C
import           Data.Default (Default(def))
import           Level
import qualified Level.Task as LevelTask
import           Renderable
import           StaticElement (StaticElement)
import           TaskManagement
import           TestHelper (createLevel)
import           Tile (TileRepr(toTile))

data GameState = GameState { _cursor :: (Int,Int)
                           , _level:: Level
                           , _taskManager :: TaskManager
                           , _messages :: [String]
                           , _counter :: Counter
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

  eventLoop vty (GameState (0,0) lvlInit empty ["Game started."] def)
  shutdown vty

  where
    addTask' task s = let (tid,s') = freshId s
                          taskManagerWithTask = addTask (task tid) (s ^. taskManager)
                          ts' = assignTasks (s ^. level) taskManagerWithTask
                      in s' & taskManager .~ ts'


    onKeyPressed _ c state@(GameState csr lvl tm _ _) =
      case c of
           '.' -> return $ logMessages state $ executeGameStep lvl tm
           'm' -> return $ case findWall csr lvl of
             Just w  -> addTask' (LevelTask.mine w lvl) state
             Nothing -> errorMessage "Mining target not mineable" state
           'h' -> return $ moveCursor state (-1) 0
           'j' -> return $ moveCursor state  0   1
           'k' -> return $ moveCursor state  0 (-1)
           'l' -> return $ moveCursor state  1   0
           _   -> return state

    moveCursor (GameState (x,y) lvl t msgs c) dx dy = GameState (x',y') lvl t msgs c
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

errorMessage :: String -> GameState -> GameState
errorMessage str = messages %~ (str :)

infoMessage :: String -> GameState -> GameState
infoMessage str = messages %~ (str :)

warnMessage :: String -> GameState -> GameState
warnMessage str = messages %~ (str :)

logMessages :: GameState -> ([AbortedTask], Level, TaskManager) -> GameState
logMessages (GameState csr _ _ msgs c) (abortedTasks, lvl, tm) = GameState csr lvl tm (abortedTaskMessages abortedTasks ++ msgs) c

abortedTaskMessages :: [AbortedTask] -> [String]
abortedTaskMessages = map (\ (AbortedTask _ msg) -> msg)

findWall :: (Int, Int) -> Level -> Maybe StaticElement
findWall c lvl = preview _head
               $ findStaticElement (\t -> render (toTile t) == '#'
                                && lvl ^. coordOf t  == from2d c) lvl

drawGame :: GameState -> Picture
drawGame s@(GameState (x,y) _ _ _ _) =
  setCursor (x,y) . pic_for_image $ (drawLevel (s ^. level) <-> drawMessages (s ^. messages))
                                <|> drawTaskManager (s ^. taskManager)

drawLevel :: Level -> Image
drawLevel = vert_cat . map (string def_attr) . lines . show

drawMessages :: [String] -> Image
drawMessages = vert_cat . map (string def_attr)

drawTaskManager :: TaskManager -> Image
drawTaskManager tm = vert_cat . map (string def_attr) $
                     [ "inactive: " ++ tm ^. inactive . folded . to show
                     , "active: " ++ tm ^. active . folded . to show
                     , "assignment: " ++ (show $ tm ^. taskAssignment)
                     ]

freshId :: GameState -> (Identifier a, GameState)
freshId state = let (ident,cnt) = C.freshId (state ^. counter)
                in (ident,state & counter .~ cnt)

setCursor :: (Int, Int) -> Picture -> Picture
setCursor (x,y) pic = pic { pic_cursor = Cursor (fromIntegral x) (fromIntegral y)}
