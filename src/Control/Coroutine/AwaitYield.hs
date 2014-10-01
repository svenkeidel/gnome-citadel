{-# LANGUAGE DeriveFunctor #-}
module Control.Coroutine.AwaitYield where

import Control.Applicative
import Control.Coroutine

data AwaitYield x y c
  = Await (x -> c)
  | Yield y c
  deriving Functor

await :: Applicative m => Coroutine (AwaitYield x y) m x
await = suspend (Await pure)

yield :: Applicative m => y -> Coroutine (AwaitYield x y) m ()
yield y = suspend (Yield y (pure ()))
