module TestTiles where

import Actor
import StaticElement

wall :: StaticElement
wall = StaticElement undefined '#'

free :: StaticElement
free = StaticElement undefined ' '

dwarf :: Actor
dwarf = Actor undefined '@' []
