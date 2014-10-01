{-# LANGUAGE DeriveFunctor #-}
module Coroutine where

import Control.Applicative

newtype Coroutine s m r = Coroutine { resume :: (m (Either (s (Coroutine s m r)) r)) }

data Pipe x y c = Pipe (x -> (y, c))
  deriving Functor

data Yield x c = Yield x c
  deriving Functor

newtype Await x c = Await (x -> c)
  deriving Functor

instance (Functor s, Functor m) => Functor (Coroutine s m) where
  fmap f (Coroutine c) = Coroutine (fmap go c)
    where
      go (Left s)  = Left (fmap (fmap f) s)
      go (Right r) = Right (f r)

instance (Functor s, Applicative m) => Applicative (Coroutine s m) where
  pure r = Coroutine (pure (Right r))
  (Coroutine f) <*> (Coroutine c) = Coroutine (fmap apply f <*> c)
    where
      apply (Right f') (Right c') = Right $ f' c'
      apply (Left f') (Left c') = Left $ fmap (<*> suspend c') f'
      apply (Left f') (Right c') = Left $ fmap (fmap ($ c')) f'
      apply (Right f') (Left c') = Left $ fmap (fmap (f' $)) c'

instance (Functor s, Monad m) => Monad (Coroutine s m) where
  return x = Coroutine (return (Right x))
  t >>= f = Coroutine (resume t >>= apply f)
    where
      apply f' (Right x) = resume (f' x)
      apply f' (Left s) = return (Left (fmap (>>= f') s))

suspend :: (Applicative m, Functor s) => s (Coroutine s m x) -> Coroutine s m x
suspend s = Coroutine (pure (Left s))

yield :: Applicative m => x -> Coroutine (Yield x) m ()
yield x = suspend (Yield x (pure ()))

await :: Applicative m => Coroutine (Await x) m x
await = suspend (Await pure)

pipe :: Applicative m => (a -> b) -> Coroutine (Pipe a b) m ()
pipe f = suspend (Pipe (\x -> ((f x), pure ())))
