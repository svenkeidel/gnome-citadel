{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module Actor ( Actor (..)
             , id
             , charRepr
             , pickItem
             , dropItem
             , abilities
             ) where

import Prelude hiding(id)

import Control.Lens.Operators
import Control.Lens.TH

import qualified Data.Set as Set

import Types
import qualified StaticElement as S

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
