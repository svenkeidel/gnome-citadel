{-# LANGUAGE TemplateHaskell #-}
module Actor ( Actor (..)
             , actorId
             , actorCharRepr
             ) where

import Control.Lens.TH
import Types

data Actor = Actor { _actorId :: Identifier
                   , _actorCharRepr :: Char
                   }
makeLenses ''Actor
