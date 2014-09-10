module TestHelper ( createLevel
                  , createLevelWithTools
                  , findDwarf
                  , findDwarfByCoord
                  , findWall
                  , isRight
                  , isLeft
                  ) where

import           Control.Lens.Extras (is)
import           Control.Lens.Fold (allOf, folded)
import           Control.Lens.Getter (view)
import           Control.Lens.Operators

import           Data.Default

import           Actor (Actor, hasAbility, TaskType(Mine), pickItem)
import           Counter
import           Level
import           Renderable
import           StaticElement (StaticElement, _Walkable)
import qualified StaticElement as S
import qualified TestTiles as T
import           Tile
import           Utils

createLevel :: String -> Level
createLevel lvlString = lvl & walkable .~ walkableFunction
  where
    lvl = fst $ fromString levelBuilder lvlString def
    walkableFunction level coord = everythingWalkable (staticElementsAt level coord)
                                   && inBounds coord level
    everythingWalkable :: [StaticElement] -> Bool
    everythingWalkable = allOf folded (is _Walkable . view S.walkable)

createLevelWithTools :: String -> Level
createLevelWithTools lvlString =
  let lvl = createLevel lvlString
      giveMiningDwarfsPickaxes :: [Level -> Level]
      giveMiningDwarfsPickaxes = zipWith givePickaxe (findActor (hasAbility Mine) lvl) [40..]
      givePickaxe miner idt lvl' = let tool = T.pickaxe (Identifier idt)
                                   in addActor (pickItem tool miner) $ addItem tool lvl'
  in giveMiningDwarfsPickaxes $$ lvl

levelBuilder :: TileBuilder
levelBuilder ident char =
  case char of
    '#' -> Just $ Right $ T.wall (asIdentifierOf ident)
    ' ' -> Just $ Right $ T.free (asIdentifierOf ident)
    'm' -> Just $ Left  $ T.miner (asIdentifierOf ident)
    'c' -> Just $ Left  $ T.chopper (asIdentifierOf ident)
    'x' -> Just $ Right $ T.pickaxe (asIdentifierOf ident)
    _   -> error ("unrecognized char " ++ show char)

findDwarf :: Level -> Char -> Actor
findDwarf lvl c = head $ findActor (\a -> render (toTile a) == c) lvl

findDwarfByCoord :: Level -> Coord -> Actor
findDwarfByCoord lvl c = head $ actorsAt lvl c

findWall :: Level -> (Int, Int) -> StaticElement
findWall lvl c = head $ findStaticElement (\t -> render (toTile t) == '#'
                                              && lvl ^. coordOfTile t  == from2d c) lvl

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

isLeft :: Either a b -> Bool
isLeft = not . isRight
