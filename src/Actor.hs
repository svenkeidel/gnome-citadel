{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module Actor ( Actor (..)
             , actorId
             , actorCharRepr
             , pickItem
             , dropItem
             ) where

import Control.Lens((%=))
import Control.Lens.TH
import Control.Monad.State
import Types

data Actor = Actor { _actorId :: Identifier
                   , _actorCharRepr :: Char
                   , _actorHoldedItems :: [Identifier]
                   } deriving Show
makeLenses ''Actor

pickItem :: MonadState Actor m => Identifier -> m ()
pickItem ident = actorHoldedItems %= (ident :)

dropItem :: MonadState Actor m => Identifier -> m ()
dropItem = undefined
