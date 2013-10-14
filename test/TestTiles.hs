module TestTiles where

import qualified Data.Set as Set

import Types
import Actor
import StaticElement

wall :: StaticElement
wall = StaticElement undefined '#'

free :: StaticElement
free = StaticElement undefined ' '

dwarf :: Actor
dwarf = Actor undefined '@' [] (Set.fromList [Mine])
