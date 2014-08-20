{-# LANGUAGE FlexibleContexts #-}
module Level.Command where

import           Control.Lens ((^.),(^?), non)
import           Control.Lens.At (contains)
import           Control.Lens.Cons (_tail)
import           Control.Lens.Fold (folded)
import           Control.Monad.Error

import           Data.Monoid

import qualified Data.Foldable as DF

import           Actor
import           Coords
import           Level
import qualified Level.Transformation as T
import Level.Transformation (LevelTrans, LevelError(PathNotFound))
import           Path (pathCoords)
import           StaticElement
import           Unfold

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
    Nothing   -> return (const . throwError $ PathNotFound fromCoord dest)
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
    , return $ T.failOnMissingItem actor item itemCoord
             >> T.pickup actor item
    ]
  where itemCoord = lvl ^. coordOf item

mine :: StaticElement -> Actor -> Level -> Command
mine block actor lvl =
  mconcat
    [ if minerHasTool
        then mempty
        else
           case findTool Mining (lvl ^. coordOf actor) lvl of
             Just t  -> pickup t actor lvl
             Nothing -> return $ const . throwError $ T.ToolMissing Mining
    , approach blockCoord actor lvl
    , return $ T.failOnMissingItem actor block blockCoord
             >> T.mine actor block
    ]
  where blockCoord = lvl ^. coordOf block
        minerHasTool :: Bool
        minerHasTool = actorInventory lvl actor ^? folded . category . contains Mining ^. non False
