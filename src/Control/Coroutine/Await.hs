{-# LANGUAGE DeriveFunctor
           , MultiParamTypeClasses
           , FunctionalDependencies
           , FlexibleInstances
           #-}
module Control.Coroutine.Await where

import Control.Applicative
import Control.Coroutine

newtype Await x c = Await (x -> c)
  deriving Functor

instance Awaiting x (Await x) where
  await = suspend (Await pure)
