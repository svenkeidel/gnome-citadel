{-# LANGUAGE RankNTypes, FlexibleContexts #-}
module Level.Commands where

import Control.Lens ((^.),(%=))
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error
import Control.Applicative

import Data.Monoid

import qualified Data.HashMap.Lazy as H
import qualified Data.List as L

import Level
import Path
import Actor
import Tile
import Utils
import Unfold

type Command = Unfold

-- | A level transformation that is used by the command scheduler
type LevelTrans m = StateT Level m ()
type LevelCommand m = Command (LevelTrans m)

data ApproachError
  = PathNotFound Actor Coord
  | ApproachError String

instance Error ApproachError where
  strMsg = ApproachError

instance Show ApproachError where
  show (PathNotFound a c) = show $ ApproachError $
    "No path could be found for the actor " ++ show a ++ " to the location " ++ show c
  show (ApproachError s)  = "ApproachError: " ++ s

-- | find a way to the destination and move the actor to it
approach :: (Applicative m1, MonadReader Level m1, MonadError ApproachError m1,
             Applicative m2, MonadError (MoveError Actor) m2)
            => Actor -> Coord -> m1 (LevelCommand m2)
approach actor dest = do
  maybePath <- join $ findPath <$> getCoord actor <*> pure dest
  case maybePath of
    Just path -> return $ mconcat [ move actor coord | coord <- safeTail $ path ^. pathCoords ]
    Nothing   -> throwError $ PathNotFound actor dest
  where
    safeTail (_:xs) = xs
    safeTail []     = []

data MoveError t
  = PathBlocked t Coord
  | MoveError String

instance Error (MoveError t) where
  strMsg = MoveError

instance Show t => Show (MoveError t) where
  show (PathBlocked t c) = show
    (MoveError $ "The destination " ++ show c ++ " for the move of " ++ show t ++ " is blocked" :: MoveError ())
  show (MoveError s)     = "MoveError: " ++ s

-- | move an actor or static element to an adjacent field
move :: (TileRepr t, Applicative m, MonadError (MoveError t) m)
     => t -> Coord -> LevelCommand m
move t dest =
  return $ do
    src <- runInState $ getCoord t
    lvl <- get
    if lvl ^-> walkable $ dest
      then do
        let idT = toTile t ^. tileId
        idToCoord %= H.insert idT dest
        coordToId %= H.adjust (L.delete idT) src
        coordToId %= H.adjust (idT :) dest
      else throwError $ PathBlocked t dest

tryCommand :: (Error e, Functor m) => Command (ErrorT e m ()) -> Command (m ())
tryCommand = fmap (void . runErrorT)
