{-# LANGUAGE DeriveFunctor
           , MultiParamTypeClasses
           , FlexibleInstances
           #-}
module Control.Coroutine.AwaitYield where

import Control.Applicative
import Control.Coroutine

data AwaitYield x y c
  = Await (x -> c)
  | Yield y c
  deriving Functor

instance Awaiting x (AwaitYield x y) where
  await = suspend (Await pure)

instance Yielding y (AwaitYield x y) where
  yield y = suspend (Yield y (pure ()))
