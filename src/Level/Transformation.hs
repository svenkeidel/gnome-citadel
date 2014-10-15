{-# LANGUAGE ViewPatterns #-}

module Level.Transformation where

import           Control.Lens ((&),(^.),ix,(%~),(.~))
import           Control.Lens.Fold (preview)
import           Control.Monad.Error

import qualified Data.Map as M

import           Coords
import           Level
import           Tile

import           Actor (Actor)
import           StaticElement (StaticElement,Category)
import qualified Actor
import qualified StaticElement

type LevelTrans = Level -> Either LevelError Level

data LevelError
  = PathBlocked Tile Coord
  | PathNotFound Coord Coord
  | ItemMissing Actor StaticElement Coord
  | TargetMissing Actor StaticElement
  | ToolMissing Category
  | ActorMissing Actor
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
  show (TargetMissing a t) = show $ OtherError $
    "The actor " ++ show a ++ " could complete the job because " ++ show t ++
    " is not at the expected location"
  show (ToolMissing c) = show $ OtherError $
    "No suitable tool for " ++ show c
  show (ActorMissing actor) = show $ OtherError $
    "Actor " ++ show actor ++ " cannot be found on the map"
  show (OtherError s)  = "LevelError: " ++ s

-- | move an actor or static element to an adjacent field
move :: TileRepr t => t -> Coord -> LevelTrans
move (toTile -> t) dest level =
  if isWalkable dest level
  then return $ level & coordOfTile t .~ dest
  else throwError $ PathBlocked t dest

-- | removes the item from the map and places it in the inventory of the actor
pickup :: Actor -> StaticElement -> LevelTrans
pickup actor item =
  return . deleteFromCoords item . (actors . ix (actor ^. Actor.id) %~ Actor.pickItem item)

-- | removes the mining target and places the actor on that field.
mine :: Actor -> StaticElement -> LevelTrans
mine actor block level =
  case preview (coordOfTile block) level of
    Nothing          -> throwError (TargetMissing actor block)
    Just blockCoords ->
      level & deleteFromCoords block
            & staticElements %~ M.delete (block ^. StaticElement.id)
            & move actor blockCoords

failOnMissingItem :: Actor -> StaticElement -> Coord -> LevelTrans
failOnMissingItem actor item oldCoord level =
  if itemPresent
     then return level
     else throwError $ ItemMissing actor item oldCoord
  where
    actualCoord = preview (coordOfTile item) level
    itemPresent = Just oldCoord == actualCoord
