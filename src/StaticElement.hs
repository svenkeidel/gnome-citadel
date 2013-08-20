{-# LANGUAGE TemplateHaskell #-}
module StaticElement ( StaticElement (..)
                     , staticElementId
                     , staticElementCharRepr
                     ) where

import Control.Lens.TH
import Types

data StaticElement = StaticElement { _staticElementId :: Identifier
                                   , _staticElementCharRepr :: Char
                                   }

makeLenses ''StaticElement
