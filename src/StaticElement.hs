{-# LANGUAGE TemplateHaskell #-}
module StaticElement ( StaticElement (..)
                     , id
                     , charRepr
                     ) where

import Prelude hiding(id)

import Control.Lens.TH
import Counter

data StaticElement = StaticElement { _id :: Identifier StaticElement
                                   , _charRepr :: Char
                                   } deriving Show

makeLenses ''StaticElement
