{-# LANGUAGE DeriveFunctor #-}
module Control.Coroutine.Await where

import Control.Applicative
import Control.Coroutine

newtype Await x c = Await (x -> c)
  deriving Functor

await :: Applicative m => Coroutine (Await x) m x
await = suspend (Await pure)
