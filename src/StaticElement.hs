{-# LANGUAGE TemplateHaskell #-}
module StaticElement ( StaticElement (..)
                     , id
                     , charRepr
                     , walkable
                     , Walkable(..)
                     , _Walkable
                     , _Blocking
                     , category
                     , Category(..)
                     ) where

import Prelude hiding(id)

import Control.Lens.TH
import Counter
import Data.Set (Set)

data Walkable = Walkable | Blocking deriving Show
makePrisms ''Walkable

data Category
  = Mining
  | WoodChopping
  | Soil
  deriving (Show, Eq, Ord)

data StaticElement = StaticElement { _id :: Identifier StaticElement
                                   , _charRepr :: Char
                                   , _walkable :: Walkable
                                   , _category :: Set Category
                                   } deriving Show

makeLenses ''StaticElement
