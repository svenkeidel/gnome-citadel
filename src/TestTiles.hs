module TestTiles where

import qualified Data.Set as Set

import Counter
import Actor
import StaticElement

wall :: Identifier StaticElement -> StaticElement
wall i = StaticElement i '#' Blocking (Set.fromList [Soil])

free :: Identifier StaticElement -> StaticElement
free i = StaticElement i ' ' Walkable Set.empty

miner :: Identifier Actor -> Actor
miner i = Actor i 'm' [] (Set.fromList [Mine])

chopper :: Identifier Actor -> Actor
chopper i = Actor i 'c' [] (Set.fromList [Lumber])

pickaxe :: Identifier StaticElement -> StaticElement
pickaxe i = StaticElement i '⚒' Walkable (Set.fromList [Mining])
