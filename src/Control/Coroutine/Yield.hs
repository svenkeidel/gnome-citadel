{-# LANGUAGE DeriveFunctor
           , MultiParamTypeClasses
           , FlexibleInstances
           #-}
module Control.Coroutine.Yield where

import Control.Applicative
import Control.Coroutine

data Yield x c = Yield x c
  deriving Functor

instance Yielding x (Yield x) where
  yield x = suspend (Yield x (pure ()))
