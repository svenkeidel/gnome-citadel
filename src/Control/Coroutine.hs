{-# LANGUAGE DeriveFunctor
           , FlexibleContexts
           , FlexibleInstances
           , UndecidableInstances
           , MultiParamTypeClasses
           , FunctionalDependencies
           #-}
{-
 - The code of this module is taken straight from Mario Blazevic's
 - monad-coroutine library, so all credits go to him. We added an applicative
 - instance for `Coroutine` and lowered the constraints on functions like yield
 - and await to applicative.
 -}
module Control.Coroutine where

import Control.Applicative
import Control.Monad.Error

newtype Coroutine s m r = Coroutine { resume :: (m (Either (s (Coroutine s m r)) r)) }

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

instance (Functor s, MonadError e m) => MonadError e (Coroutine s m) where
  throwError e = Coroutine (throwError e)
  catchError (Coroutine c) k = Coroutine (catchError c (resume . k))

instance Functor s => MonadTrans (Coroutine s) where
  lift = Coroutine . liftM Right

class Yielding x f | f -> x where
  yield :: Applicative g => x -> Coroutine f g ()

class Awaiting x f | f -> x where
  await :: Applicative g => Coroutine f g x

suspend :: (Applicative m, Functor s) => s (Coroutine s m x) -> Coroutine s m x
suspend s = Coroutine (pure (Left s))
