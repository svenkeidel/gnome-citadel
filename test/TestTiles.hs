module TestTiles where

import qualified Data.Set as Set

import Counter
import Actor
import StaticElement

wall :: Identifier StaticElement -> StaticElement
wall i = StaticElement i '#'

free :: Identifier StaticElement -> StaticElement
free i = StaticElement i ' '

dwarf :: Identifier Actor -> Actor
dwarf i = Actor i '@' [] (Set.fromList [Mine])
