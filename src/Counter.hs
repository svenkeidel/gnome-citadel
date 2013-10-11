{-# LANGUAGE TemplateHaskell, RankNTypes, FlexibleContexts #-}
module Counter ( Counter
               , freshId
               ) where

import Control.Lens ((<+=))
import Control.Lens.TH
import Control.Monad.State

import Data.Default

import Types

newtype Counter = Counter { _counter :: Identifier }
makeLenses ''Counter

freshId :: MonadState Counter m => m Identifier
freshId = counter <+= 1

instance Default Counter where
  def = Counter 0
