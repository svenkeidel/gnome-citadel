{-# LANGUAGE RankNTypes,
             ExistentialQuantification,
             FlexibleInstances,
             MultiParamTypeClasses,
             UndecidableInstances #-}
module Unfold ( Unfoldable(..)
              , Step(..)
              , Unfold
              , UnfoldT(..)
              , unfold
              , unfoldList
              , toList
              , fromList
              ) where

import Data.Monoid
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error
import qualified Data.Foldable as F
import qualified Data.Traversable as T

class Unfoldable u where
  next :: u a -> Step (u a) a

data Step s a
  = Yield a s
  | Done

instance Functor (Step s) where
  fmap f (Yield a s) = Yield (f a) s
  fmap _ Done        = Done

instance F.Foldable (Step s) where
  foldMap f (Yield a _) = f a
  foldMap _ Done        = mempty

instance T.Traversable (Step s) where
  traverse f (Yield a s) = Yield <$> f a <*> pure s
  traverse _ Done        = pure Done

infixl 4 <!>
(<!>) :: (s -> t) -> Step s a -> Step t a
f <!> (Yield a s) = Yield a $ f s
_ <!> Done        = Done

data UnfoldS s a
  -- | An 'UnfoldS s a' has a state 's' and produces values of type 'a'.
  = UnfoldS s (s -> Step s a)

instance Unfoldable (UnfoldS s) where
  next (UnfoldS s next') = (\s' -> UnfoldS s' next') <!> next' s

instance Functor (UnfoldS s) where
  fmap f (UnfoldS s next') = UnfoldS s $ fmap f . next'

data Unfold a
  -- | An 'Unfold' wraps an 'UnfoldS' and hides its state.
  = forall u. (Functor u, Unfoldable u) => Unfold (u a)

-- | Smart constructor for an 'Unfold'
unfold :: s -> (s -> Step s a) -> Unfold a
unfold s next' = Unfold $ UnfoldS s next'

instance Eq a => Eq (Unfold a) where
  u1 == u2 = toList u1 == toList u2

instance Show a => Show (Unfold a) where
  show u = "Unfold " ++ show (toList u)

instance Unfoldable Unfold where
  next (Unfold u) = Unfold <!> next u

instance Monoid (Unfold a) where
  mempty = unfold () (const Done)
  u1 `mappend` u2 =
    unfold (Left u1) next'
    where
      next' (Left u1') =
        case next u1' of
          Done         -> next' (Right u2)
          Yield a u1'' -> Yield a (Left u1'')
      next' (Right u2') = Right <!> next u2'

instance Functor Unfold where
  fmap f (Unfold u) = Unfold (fmap f u)

instance Applicative Unfold where
  pure  = return
  (<*>) = ap

instance Monad Unfold where
  return x = unfold True (\s -> if s then Yield x False else Done)
  u0 >>= f = unfold (Left u0) next'
    where
      next' (Left u1) =
        case next u1 of
          Yield a u1' -> next' $ Right (u1',f a)
          Done      -> Done
      next' (Right (u1,u2)) =
        case next u2 of
          Yield a u2' -> Yield a (Right (u1,u2'))
          Done        -> next' $ Left u1

instance F.Foldable Unfold where
  foldr f b u =
    case next u of
      Yield a u' -> F.foldr f (f a b) u'
      Done       -> b

instance T.Traversable Unfold where
  -- (a -> f b) -> Unfold a -> f (Unfold b)
  traverse f u =
    case next u of
      Done       -> pure mempty
      Yield a u' -> mappend <$> (pure <$> f a) <*> T.traverse f u'

toList :: Unfold a -> [a]
toList = reverse . F.foldr (:) []

fromList :: [a] -> Unfold a
fromList = flip unfold unfoldList

unfoldList :: [a] -> Step [a] a
unfoldList (x:xs) = Yield x xs
unfoldList []     = Done

newtype UnfoldT m a = UnfoldT { runUnfoldT :: m (Unfold a) }

instance Functor m => Functor (UnfoldT m) where
  fmap f = UnfoldT . fmap (fmap f) . runUnfoldT

instance (Applicative m, Monad m) => Applicative (UnfoldT m) where
  pure  = return
  (<*>) = ap

instance (Applicative m, Monad m) => Monad (UnfoldT m) where
  return = UnfoldT  . return . return
  u >>= f = UnfoldT $ liftM join $ T.traverse (runUnfoldT . f) =<< runUnfoldT u

instance Applicative m => Monoid (UnfoldT m a) where
  mempty          = UnfoldT $ pure mempty
  u1 `mappend` u2 = UnfoldT $ mappend <$> runUnfoldT u1 <*> runUnfoldT u2

instance MonadTrans UnfoldT where
  lift m = UnfoldT (return `liftM` m)

mapUnfoldT :: (m (Unfold a) -> n (Unfold b)) -> UnfoldT m a -> UnfoldT n b
mapUnfoldT f = UnfoldT . f . runUnfoldT

instance (Applicative m, MonadReader r m) => MonadReader r (UnfoldT m) where
  ask    = lift ask
  local  = mapUnfoldT . local
  reader = lift . reader

instance (Applicative m, MonadState s m) => MonadState s (UnfoldT m) where
  get   = lift get
  put   = lift . put
  state = lift . state


liftCatch :: (m (Unfold a) -> (e -> m (Unfold a)) -> m (Unfold a)) ->
    UnfoldT m a -> (e -> UnfoldT m a) -> UnfoldT m a
liftCatch f m h = UnfoldT $ f (runUnfoldT m) (runUnfoldT . h)

instance (Applicative m, MonadError e m) => MonadError e (UnfoldT m) where
  throwError = lift . throwError
  catchError = liftCatch catchError
