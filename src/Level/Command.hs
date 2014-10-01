{-# LANGUAGE FlexibleContexts #-}
module Level.Command where

import           Control.Coroutine
import           Control.Coroutine.AwaitYield
import           Control.Lens ((^.),(^?), non,view)
import           Control.Lens.At (contains)
import           Control.Lens.Cons (_tail)
import           Control.Lens.Fold (folded)
import           Control.Monad.Error

import           Actor
import           Coords
import           Level
import qualified Level.Transformation as T
import Level.Transformation (LevelTrans, LevelError(PathNotFound))
import           Path (pathCoords)
import           StaticElement

type Command = Coroutine (AwaitYield Level Level) (Either LevelError) ()

runTransformation :: LevelTrans -> Command
runTransformation f = do
  lvl <- await
  lvl' <- lift $ f lvl
  yield lvl'

-- | find a way to the destination and move the actor to it
approach :: Coord -> Actor -> Command
approach dest actor = do
  lvl <- await
  let fromCoord  = lvl ^. coordOfTile actor
      maybePath  = findArea fromCoord destCoords lvl
      destCoords = if isWalkable dest lvl
                   then [dest]
                   else filter (`isWalkable` lvl) (neighbors2d dest)
  case maybePath of
    Just path -> go (path ^. pathCoords . _tail)
    Nothing   -> throwError $ PathNotFound fromCoord dest
  where
    go []       = return ()
    go (p:path) = do
      runTransformation $ T.move actor p
      go path

pickup :: StaticElement -> Actor -> Command
pickup item actor = do
  lvl <- await
  let itemCoord = lvl ^. coordOfTile item
  approach itemCoord actor
  failOnMissingItem actor item itemCoord
  runTransformation $ T.pickup actor item

failOnMissingItem :: Actor -> StaticElement -> Coord -> Command
failOnMissingItem actor item oldCoord = do
  lvl <- await
  let actualCoord = view (coordOfTile item) lvl
      itemPresent = oldCoord == actualCoord
  unless itemPresent $ throwError $ T.ItemMissing actor item oldCoord

mine :: StaticElement -> Actor -> Command
mine block actor = do
  lvl <- await
  let minerHasTool = actorInventory lvl actor ^? folded . category . contains Mining ^. non False
  unless minerHasTool $
    case findTool Mining (lvl ^. coordOfTile actor) lvl of
      Just t  -> pickup t actor
      Nothing -> throwError $ T.ToolMissing Mining

  let blockCoord = lvl ^. coordOfTile block
  approach blockCoord actor
  failOnMissingItem actor block blockCoord
  runTransformation $ T.mine actor block
