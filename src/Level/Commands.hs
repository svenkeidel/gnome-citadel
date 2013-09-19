{-# LANGUAGE RankNTypes, FlexibleContexts, ViewPatterns #-}
module Level.Commands where

import Control.Lens ((^.),(%=),(.=),use,view,contains,ix)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error
import Control.Applicative

import Safe (tailSafe)
import Data.Monoid

import Level
import Path(pathCoords)
import Actor
import StaticElement
import Tile
import Unfold

-- | A level transformation that is used by the command scheduler
type LevelTrans = StateT Level (Either LevelError) ()
type CommandT m = UnfoldT m LevelTrans
type Command = Unfold LevelTrans

commandT :: Monad m => Command -> CommandT m
commandT = UnfoldT . return

runCommandT :: CommandT m -> m Command
runCommandT = runUnfoldT

data LevelError
  = PathBlocked Tile Coord
  | ItemMissing Actor StaticElement Coord
  | LevelError String

instance Error LevelError where
  strMsg = LevelError

instance Show LevelError where
  show (PathBlocked t c) = show $ LevelError $
    "The destination " ++ show c ++ " for the move of " ++ show t ++ " is blocked"
  show (ItemMissing a i c) = show $ LevelError $
    "The actor " ++ show a ++ " could not pickup the item " ++ show i ++
    " because it is not at the specified location " ++ show c
  show (LevelError s)  = "LevelError: " ++ s

data ApproachError
  = PathNotFound Actor Coord
  | ApproachError String

instance Error ApproachError where
  strMsg = ApproachError

instance Show ApproachError where
  show (PathNotFound a c) = show $ LevelError $
    "No path could be found for the actor " ++ show a ++ " to the location " ++ show c
  show (ApproachError s)  = s

-- | find a way to the destination and move the actor to it
approach :: (Applicative m, MonadReader Level m, MonadError ApproachError m)
         => Actor -> Coord -> CommandT m
approach actor dest = do
  maybePath <- view =<< findArea <$> view (coordOf actor) <*> destCoords
  case maybePath of
    Just path -> commandT $ mconcat [ move actor coord | coord <- tailSafe $ path ^. pathCoords ]
    Nothing   -> throwError $ PathNotFound actor dest
  where
    destCoords = do
      destIsWalkable <- view $ isWalkable dest
      if destIsWalkable
        then return $ [dest]
        else filterM (view . isWalkable) [ dest |+| from2d (dx, dy) | dx <- [-1,0,1], dy <- [-1,0,1] ]

-- | move an actor or static element to an adjacent field
move :: (TileRepr t) => t -> Coord -> Command
move (toTile -> t) dest =
  return $ do
    walkable' <- use $ isWalkable dest
    if walkable'
      then coordOf t .= dest
      else throwError $ PathBlocked t dest

pickup :: (Applicative m, MonadReader Level m, MonadError ApproachError m)
       => Actor -> StaticElement -> CommandT m
pickup actor item = do
  itemCoord <- view $ coordOf item
  mconcat
    [ approach actor itemCoord
    , commandT $ return $ do
        itemPresent <- use $ idToCoord . contains itemId
        if itemPresent
          then do
            actors . ix idActor %= execState (pickItem itemId)
            deleteFromCoords item
          else throwError $ ItemMissing actor item itemCoord
    ]
  where
    itemId  = item ^. staticElementId
    idActor = actor ^. actorId
