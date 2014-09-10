{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import           Control.Applicative ((<$>),(<*>))
import           Control.Lens (_1, _2, preview, _head, folded, to, view)
import           Control.Lens.Operators
import           Control.Lens.TH
import           Data.Default (Default(def))
import           Data.Maybe (catMaybes)
import           Data.Traversable (traverse)
import           Graphics.Vty

import           Counter (Counter, Identifier)
import qualified Counter as C
import           Level
import qualified Level.Task as LevelTask
import           Renderable
import           StaticElement (StaticElement)
import           Task (Task)
import           TaskManagement (AbortedTask(..), TaskManager, addTask, assignTasks, empty)
import qualified TaskManagement as TM
import           TestHelper (createLevel)
import           Tile (TileRepr(toTile))
import           Utils

data GameState = GameState { _cursor :: (Int,Int)
                           , _level:: Level
                           , _taskManager :: TaskManager
                           , _messages :: [String]
                           , _counter :: Counter
                           }
makeLenses ''GameState

instance Default GameState where
  def = GameState def emptyLevel empty ["Game started."] def

main :: IO ()
main = do
  vty <- mkVty def
  eventLoop vty (def & level .~ lvlInit)
  shutdown vty

lvlInit :: Level
lvlInit = createLevel $ unlines startLevel

onKeyPressed :: Monad m => t -> Char -> GameState -> m GameState
onKeyPressed _ c state = case c of
  '.' -> return $ executeGameStep state
  ':' -> return $ nTimes 50 executeGameStep state
  'm' -> return $ case findWall csr lvl of
    Just w  -> addTask' (LevelTask.mine w lvl) state
    Nothing -> errorMessage "Mining target not mineable" state
  'M' -> do
    let ws = findAllWalls lvl
        fs = map (\w -> addTask' $ LevelTask.mine w lvl) ws
    return $ fs $$ state
  'h' -> return $ moveCursor state (-1) 0
  'j' -> return $ moveCursor state  0   1
  'k' -> return $ moveCursor state  0 (-1)
  'l' -> return $ moveCursor state  1   0
  'r' -> return (def & level .~ lvlInit)
  _   -> return state
  where csr = state ^. cursor
        lvl = state ^. level

nTimes :: Int -> (a -> a) -> a -> a
nTimes 0 _ = id
nTimes n f = f . nTimes (n-1) f

executeGameStep :: GameState -> GameState
executeGameStep s = s & level .~ lvl' & taskManager .~ tm' & logMessages aborted
  where
    (aborted, lvl', tm') = TM.executeGameStep <$> view level <*> view taskManager $ s

moveCursor :: GameState -> Int -> Int -> GameState
moveCursor s dx dy = s & cursor .~ (x',y')
   where
       x' = min (max 0 (x + dx)) (lvl ^. bounds . _1)
       y' = min (max 0 (y + dy)) (lvl ^. bounds . _2)
       (x,y) = s ^. cursor
       lvl = s ^. level

eventLoop :: Vty -> GameState -> IO ()
eventLoop vty state = do
  update vty (drawGame state)
  e <- nextEvent vty
  case e of
    EvKey k _ ->
      case k of
        (KChar 'q') -> return ()
        (KChar c)   -> do
          state' <- onKeyPressed vty c state
          eventLoop vty state'
        _ -> eventLoop vty state
    _ -> eventLoop vty state

addTask' :: (Identifier a -> Task) -> GameState -> GameState
addTask' task s = let (tid,s') = freshId s
                      taskManagerWithTask = addTask (task tid) (s ^. taskManager)
                      ts' = assignTasks (s ^. level) taskManagerWithTask
                  in s' & taskManager .~ ts'

errorMessage :: String -> GameState -> GameState
errorMessage str = messages %~ (str :)

infoMessage :: String -> GameState -> GameState
infoMessage str = messages %~ (str :)

warnMessage :: String -> GameState -> GameState
warnMessage str = messages %~ (str :)

logMessages :: [AbortedTask] -> GameState -> GameState
logMessages abortedTasks = messages %~ (abortedTaskMessages abortedTasks ++)

abortedTaskMessages :: [AbortedTask] -> [String]
abortedTaskMessages = map (\(AbortedTask _ msg) -> msg)

findWall :: (Int, Int) -> Level -> Maybe StaticElement
findWall c lvl = preview _head . flip findStaticElement lvl $ \t ->
  render (toTile t) == '#' && lvl ^. coordOfTile t  == from2d c

findAllWalls :: Level -> [StaticElement]
findAllWalls lvl = catMaybes $ traverse findWall cs lvl
  where cs = [(cdx,cdy) | cdx <- [0..lvl^.bounds._1], cdy <- [0..lvl^.bounds._2]]

drawGame :: GameState -> Picture
drawGame s = setCursor (s ^. cursor) . picForImage $
             (lvl' <-> msg) <|> vSep <|> tm
  where lvl = drawLevel (s ^. level)
        lvl' = lvl <-> hSep
        msg = drawMessages (s ^. messages)
        tm =drawTaskManager (s ^. taskManager)
        vSep = vline $ imageHeight lvl'
        hSep = hline $ imageWidth lvl

drawLevel :: Level -> Image
drawLevel = vertCat . map (string defAttr) . lines . show

drawMessages :: [String] -> Image
drawMessages = vertCat . map (string defAttr)

drawTaskManager :: TaskManager -> Image
drawTaskManager tm = vertCat . map (string defAttr) $
                     [ "inactive: " ++ tm ^. TM.inactive . folded . to show
                     , "active: " ++ tm ^. TM.active . folded . to show
                     , "assignment: " ++ show (tm ^. TM.taskAssignment)
                     ]

freshId :: GameState -> (Identifier a, GameState)
freshId state = (ident,state & counter .~ cnt)
  where (ident,cnt) = C.freshId (state ^. counter)

setCursor :: (Int, Int) -> Picture -> Picture
setCursor (x,y) pic = pic { picCursor = Cursor (fromIntegral x) (fromIntegral y)}

hline :: Integral d => d -> Image
hline n = charFill defAttr '━' n 1

vline :: Integral d => d -> Image
vline = charFill defAttr '┃' 1

startLevel :: [String]
startLevel =
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
