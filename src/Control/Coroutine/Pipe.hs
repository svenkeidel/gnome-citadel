{-# LANGUAGE DeriveFunctor #-}
module Control.Coroutine.Pipe where

import Control.Applicative
import Control.Coroutine

data Pipe x y c = Pipe (x -> (y, c))
  deriving Functor

pipe :: Applicative m => (a -> b) -> Coroutine (Pipe a b) m ()
pipe f = suspend (Pipe (\x -> ((f x), pure ())))
