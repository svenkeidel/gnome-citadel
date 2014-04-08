{-# LANGUAGE FlexibleContexts #-}
module Level.Command where

import Control.Lens ((^.))
import Control.Lens.Cons (_tail)

import Data.Monoid

import qualified Data.Foldable as DF

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
approach :: Coord -> Actor -> Level -> Command
approach dest actor lvl =
  case maybePath of
    Just path -> DF.foldMap (return . T.move actor) (path ^. pathCoords . _tail)
    Nothing   -> return (const . LevelError $ PathNotFound fromCoord dest)
  where
    fromCoord  = lvl ^. coordOf actor
    maybePath  = findArea fromCoord destCoords lvl
    destCoords = if isWalkable dest lvl
                 then [dest]
                 else filter (`isWalkable` lvl) (neighbors2d dest)

pickup :: StaticElement -> Actor -> Level -> Command
pickup item actor lvl =
  mconcat
    [ approach itemCoord actor lvl
    , return $ failOnMissingItem actor item itemCoord
             >> T.pickup actor item
    ]
  where itemCoord = lvl ^. coordOf item

mine :: StaticElement -> Actor -> Level -> Command
mine block actor lvl =
  mconcat
    [ approach blockCoord actor lvl
    , return $ failOnMissingItem actor block blockCoord
             >> T.mine actor block
    ]
  where blockCoord = lvl ^. coordOf block
