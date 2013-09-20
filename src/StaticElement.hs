{-# LANGUAGE TemplateHaskell #-}
module StaticElement ( StaticElement (..)
                     , id
                     , charRepr
                     ) where

import Prelude hiding(id)

import Control.Lens.TH
import Types

data StaticElement = StaticElement { _id :: Identifier
                                   , _charRepr :: Char
                                   } deriving Show

makeLenses ''StaticElement
