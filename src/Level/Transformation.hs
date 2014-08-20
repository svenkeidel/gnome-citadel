{-# LANGUAGE ViewPatterns #-}

module Level.Transformation where

import Control.Lens((&),(^.),ix,view,(%~),(.~))
import Control.Monad.Error

import qualified Data.Map as M

import Coords
import Level
import Tile

import Actor (Actor)
import StaticElement (StaticElement,Category)
import qualified Actor
import qualified StaticElement

type LevelTrans = Level -> (Either LevelError Level)

data LevelError
  = PathBlocked Tile Coord
  | PathNotFound Coord Coord
  | ItemMissing Actor StaticElement Coord
  | ToolMissing Category
  | OtherError String

instance Error LevelError where
  strMsg = OtherError

instance Show LevelError where
  show (PathBlocked t c) = show $ OtherError $
    "The destination " ++ show c ++ " for the move of " ++ show t ++ " is blocked"
  show (PathNotFound from to) = show $ OtherError $
    "There exists no path from " ++ show from ++ " to " ++ show to
  show (ItemMissing a i c) = show $ OtherError $
    "The actor " ++ show a ++ " could not pickup the item " ++ show i ++
    " because it is not at the specified location " ++ show c
  show (ToolMissing c) = show $ OtherError $
    "No suitable tool for " ++ show c
  show (OtherError s)  = "LevelError: " ++ s


-- | move an actor or static element to an adjacent field
move :: TileRepr t => t -> Coord -> LevelTrans
move (toTile -> t) dest level =
  if isWalkable dest level
  then return $ level & coordOf t .~ dest
  else throwError $ PathBlocked t dest

-- | removes the item from the map and places it in the inventory of the actor
pickup :: Actor -> StaticElement -> LevelTrans
pickup actor item =
  return . deleteFromCoords item . (actors . ix (actor ^. Actor.id) %~ Actor.pickItem item)

-- | removes the mining target and places the actor on that field.
mine :: Actor -> StaticElement -> LevelTrans
mine actor block level =
    level & deleteFromCoords block
          & staticElements %~ M.delete (block ^. StaticElement.id)
          & move actor blockCoords
  where blockCoords = view (coordOf block) level


failOnMissingItem :: Actor -> StaticElement -> Coord -> LevelTrans
failOnMissingItem actor item oldCoord level =
  if itemPresent
     then return level
     else throwError $ ItemMissing actor item oldCoord
  where
    actualCoord = view (coordOf item) level
    itemPresent = oldCoord == actualCoord
