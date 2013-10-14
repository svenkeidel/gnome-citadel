{-# LANGUAGE FlexibleContexts #-}
module Level.Command where

import Control.Lens ((^.))
import Control.Lens.Cons (_tail)
import Control.Monad.Error
import Control.Applicative

import Data.Monoid

import qualified Data.Traversable as DT

import Actor
import Coords
import Level
import Level.Transformation
import Path(pathCoords)
import StaticElement
import Unfold

import qualified Level.Transformation as T

type Command = Unfold LevelTrans

command :: Monad m => LevelTrans -> CommandT m
command = commandT . return

-- | A level transformation that is used by the command scheduler
type CommandT m = UnfoldT m LevelTrans

commandT :: Monad m => Command -> CommandT m
commandT = UnfoldT . return

runCommandT :: CommandT m -> m Command
runCommandT = runUnfoldT

-- | find a way to the destination and move the actor to it
approach :: (Applicative m, MonadError LevelError m)
         => Actor -> Coord -> Level -> CommandT m
approach actor dest lvl = do
  case maybePath of
    Just path -> commandT $ DT.foldMapDefault (return . T.move actor) (path ^. pathCoords . _tail)
    Nothing   -> throwError $ PathNotFound fromCoord dest
  where
    fromCoord  = lvl ^. coordOf actor
    maybePath  = findArea fromCoord destCoords lvl
    destCoords = if isWalkable dest lvl
                 then [dest]
                 else filter (flip isWalkable lvl) (neighbors2d dest)

pickup :: (Applicative m, MonadError LevelError m)
       => Actor -> StaticElement -> Level -> CommandT m
pickup actor item lvl = do
  mconcat
    [ approach actor itemCoord lvl
    , command $ failOnMissingItem actor item itemCoord
             >> T.pickup actor item
    ]
  where itemCoord = lvl ^. coordOf item

mine :: (Applicative m, MonadError LevelError m)
     => StaticElement -> Actor -> Level -> CommandT m
mine block actor lvl = do
  mconcat
    [ approach actor blockCoord lvl
    , command $ failOnMissingItem actor block blockCoord
             >> T.mine actor block
    ]
  where blockCoord = lvl ^. coordOf block
