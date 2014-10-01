{-# LANGUAGE DeriveFunctor #-}
module Control.Coroutine.Yield where

import Control.Applicative
import Control.Coroutine

data Yield x c = Yield x c
  deriving Functor

yield :: Applicative m => x -> Coroutine (Yield x) m ()
yield x = suspend (Yield x (pure ()))
