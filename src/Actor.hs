{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module Actor ( Actor (..)
             , id
             , charRepr
             , pickItem
             , dropItem
             ) where

import Prelude hiding(id)

import Control.Lens((%=),(^.))
import Control.Lens.TH
import Control.Monad.State
import Types
import qualified StaticElement as S

data Actor = Actor { _id :: Identifier
                   , _charRepr :: Char
                   , _inventory :: [Identifier]
                   } deriving Show
makeLenses ''Actor

pickItem :: MonadState Actor m => S.StaticElement -> m ()
pickItem item = inventory %= (item ^. S.id :)

dropItem :: MonadState Actor m => Identifier -> m ()
dropItem = undefined
