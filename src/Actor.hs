{-# LANGUAGE TemplateHaskell, FlexibleContexts, TypeFamilies #-}
module Actor ( Actor (..)
             , TaskType(..)
             , id
             , charRepr
             , pickItem
             , dropItem
             , abilities
             , inventory
             , hasAbility
             ) where

import           Control.Lens.Fold (folded, elemOf)
import Prelude hiding(id)
import           Data.Function (on)

import           Control.Lens.Operators
import           Control.Lens.TH

import qualified Data.Set as Set

import           Counter
import qualified StaticElement as S
import Control.DeepSeq (NFData, rnf)

data TaskType = Mine | Lumber deriving (Show,Eq,Ord)

instance NFData TaskType where
  rnf Mine = ()
  rnf Lumber = ()

data Actor = Actor { _id :: Identifier Actor
                   , _charRepr :: Char
                   , _inventory :: [Identifier S.StaticElement]
                   , _abilities :: Set.Set TaskType
                   }
makeLenses ''Actor

instance Symbol Actor where
  sym _ = "Actor"

instance Show Actor where
  show (Actor i c inv ab) = unwords [ "Actor {"
                                    , "id =", show i 
                                    , ", char =", show c
                                    , ", inventory =", show inv
                                    , ", abilities =", show ab
                                    , "}"
                                    ]


instance NFData Actor where
  rnf (Actor i c inv ab) = rnf (i,c,inv,ab)

instance Eq Actor where
  (==) = (==) `on` _id

instance HasIdentifier Actor where
  type Identifiable Actor = Actor
  getIdentifier = _id

pickItem :: S.StaticElement -> Actor -> Actor
pickItem item actor = actor & inventory %~ (item ^. S.id :)

dropItem :: Identifier S.StaticElement -> Actor -> Actor
dropItem = undefined

hasAbility :: TaskType -> Actor -> Bool
hasAbility = elemOf (abilities . folded)
