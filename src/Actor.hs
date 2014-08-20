{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
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

import           Control.Lens.Operators
import           Control.Lens.TH

import qualified Data.Set as Set

import           Counter
import qualified StaticElement as S

data TaskType = Mine | Lumber deriving (Show,Eq,Ord)

data Actor = Actor { _id :: Identifier Actor
                   , _charRepr :: Char
                   , _inventory :: [Identifier S.StaticElement]
                   , _abilities :: Set.Set TaskType
                   } deriving Show
makeLenses ''Actor

pickItem :: S.StaticElement -> Actor -> Actor
pickItem item actor = actor & inventory %~ (item ^. S.id :)

dropItem :: Identifier S.StaticElement -> Actor -> Actor
dropItem = undefined

hasAbility :: TaskType -> Actor -> Bool
hasAbility = elemOf (abilities . folded)
