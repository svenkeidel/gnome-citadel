{-# LANGUAGE ViewPatterns #-}
module Level.Transformation where

import Control.Lens((^.),(.=),(%=),use,ix)
import Control.Monad.Error
import Control.Monad.State

import qualified Data.Map as M

import Coords
import Level
import Tile

import Actor (Actor)
import StaticElement (StaticElement)
import qualified Actor as Actor
import qualified StaticElement as StaticElement

type LevelTrans m = StateT Level (ErrorT LevelError m) ()

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


-- | move an actor or static element to an adjacent field
move :: (Monad m, TileRepr t) => t -> Coord -> LevelTrans m
move (toTile -> t) dest = do
  walkable' <- use $ isWalkable dest
  if walkable'
    then coordOf t .= dest
    else throwError $ PathBlocked t dest

-- | removes the item from the map and places it in the inventory of the actor
pickup :: Monad m => Actor -> StaticElement -> LevelTrans m
pickup actor item = do
  actors . ix (actor ^. Actor.id) %= execState (Actor.pickItem item)
  deleteFromCoords item

-- | removes the mining target and places the actor on that field.
mine :: Monad m => Actor -> StaticElement -> LevelTrans m
mine actor block = do
  blockCoord <- use $ coordOf block
  deleteFromCoords block
  staticElements %= M.delete (block ^. StaticElement.id)
  move actor blockCoord

failOnMissingItem :: (Monad m) => Actor -> StaticElement -> Coord -> LevelTrans m
failOnMissingItem actor item oldCoord = do
  actualCoord <- use $ coordOf item
  let itemPresent = oldCoord == actualCoord
  unless itemPresent $ throwError $ ItemMissing actor item oldCoord