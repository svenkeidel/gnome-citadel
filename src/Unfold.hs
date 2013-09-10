{-# LANGUAGE RankNTypes, ExistentialQuantification #-}
module Unfold ( Unfoldable(..)
              , Step(..)
              , Unfold
              , unfold
              , unfoldList
              , toList
              ) where

import Data.Monoid
import Control.Applicative
import Control.Monad
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
  traverse f u = unfold <$> T.sequenceA (F.foldMap (pure . f) u) <*> pure unfoldList

toList :: Unfold a -> [a]
toList = reverse . F.foldr (:) []

unfoldList :: [a] -> Step [a] a
unfoldList (x:xs) = Yield x xs
unfoldList []     = Done