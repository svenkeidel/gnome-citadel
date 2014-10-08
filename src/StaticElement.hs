{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
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
import Control.DeepSeq (NFData, rnf)

data Walkable = Walkable | Blocking deriving Show
makePrisms ''Walkable

instance NFData Walkable where
  rnf Walkable = ()
  rnf Blocking = ()

data Category
  = Mining
  | WoodChopping
  | Soil
  deriving (Show, Eq, Ord)

instance NFData Category where
  rnf Mining = ()
  rnf WoodChopping = ()
  rnf Soil = ()

data StaticElement = StaticElement { _id :: Identifier StaticElement
                                   , _charRepr :: Char
                                   , _walkable :: Walkable
                                   , _category :: Set Category
                                   } deriving Show

instance NFData StaticElement where
  rnf (StaticElement i c w cat) = rnf (i,c,w,cat)

instance HasIdentifier StaticElement where
  type Identifiable StaticElement = StaticElement
  getIdentifier = _id

makeLenses ''StaticElement
