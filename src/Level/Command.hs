{-# LANGUAGE FlexibleContexts #-}
module Level.Command where

import Control.Lens ((^.),view)
import Control.Monad.Error
import Control.Monad.Reader
import Control.Applicative

import Safe (tailSafe)
import Data.Monoid
import Data.Functor.Identity

import qualified Data.Traversable as DT

import Actor
import Coords
import Level
import Level.Transformation
import Path(pathCoords)
import StaticElement
import Unfold

import qualified Level.Transformation as T

type Command = Unfold (LevelTrans Identity)

command :: Monad m => LevelTrans Identity -> CommandT m
command = commandT . return

-- | A level transformation that is used by the command scheduler
type CommandT m = UnfoldT m (LevelTrans Identity)

commandT :: Monad m => Command -> CommandT m
commandT = UnfoldT . return

runCommandT :: CommandT m -> m Command
runCommandT = runUnfoldT

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
    Just path -> commandT $ DT.foldMapDefault (return . T.move actor) (tailSafe $ path ^. pathCoords)
    Nothing   -> throwError $ PathNotFound actor dest
  where
    destCoords = do
      destIsWalkable <- view $ isWalkable dest
      if destIsWalkable
        then return $ [dest]
        else filterM (view . isWalkable) [ dest |+| from2d (dx, dy) | dx <- [-1,0,1], dy <- [-1,0,1] ]


pickup :: (Applicative m, MonadReader Level m, MonadError ApproachError m)
       => Actor -> StaticElement -> CommandT m
pickup actor item = do
  itemCoord <- view $ coordOf item
  mconcat
    [ approach actor itemCoord
    , command $ failOnMissingItem actor item itemCoord
             >> T.pickup actor item
    ]

mine :: (Applicative m, MonadReader Level m, MonadError ApproachError m)
     => Actor -> StaticElement -> CommandT m
mine actor block = do
  blockCoord <- view $ coordOf block
  mconcat
    [ approach actor blockCoord
    , command $ failOnMissingItem actor block blockCoord
             >> T.mine actor block
    ]
