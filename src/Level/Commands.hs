{-# LANGUAGE RankNTypes, FlexibleContexts #-}
module Level.Commands where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error
import Control.Applicative
import Control.Lens ((^.))
import Data.Monoid

import Unfold
import Level
import Path

type Command a = Unfold a

data ApproachError
  = PathNotFound Identifier Coord
  | ApproachError String

instance Error ApproachError where
  strMsg = ApproachError

instance Show ApproachError where

-- | find a way to the destination and move the actor to it
approach :: (Applicative m1, MonadReader Level m1, MonadError ApproachError m1,
             Applicative m2, MonadState  Level m2, MonadError MoveError m2)
            => Identifier -> Coord -> m1 (Command (m2 ()))
approach actor dest = do
  maybePath <- join $ findPath <$> getCoord actor <*> pure dest
  case maybePath of
    Just path -> return $ mconcat [ move actor coord | coord <- path ^. pathCoords ]
    Nothing   -> throwError $ PathNotFound actor dest

data MoveError
  = PathBlocked Identifier Coord
  | MoveError String

instance Error MoveError where
  strMsg = MoveError

instance Show MoveError where
  show (PathBlocked i c) = show $ MoveError $ "The path for the actor " ++ show i ++ " to " ++ show c ++ " is blocked"
  show (MoveError s)     = "MoveError: " ++ s

-- | move an actor to an adjacent field
move :: (Applicative m, MonadState Level m, MonadError MoveError m)
     => Identifier -> Coord -> Command (m ())
move actor dest =
  return $ do
    walkable' <- walkable <$> get <*> pure dest
    if walkable'
      then undefined
      else throwError $ PathBlocked actor dest

addCommand :: (MonadState Level m) => Command (m ()) -> m ()
addCommand = undefined

executeGameStep :: MonadState Level m => m ()
executeGameStep = undefined
