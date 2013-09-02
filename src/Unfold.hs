{-# LANGUAGE ExistentialQuantification #-}
module Unfold ( Unfoldable(..)
              , Exec
              , execution
              , Step(..)
              ) where

import Data.Monoid

class Unfoldable u where
  unfold :: u a -> Step (u a) a

instance Unfoldable (Unfold s) where
  unfold u = (\s -> u { state = s }) <!> (stepper u) (state u)

data Step s a
  = Yield a s
  | Done

instance Functor (Step s) where
  fmap f (Yield a s) = Yield (f a) s
  fmap _ Done        = Done

(<!>) :: (s -> t) -> Step s a -> Step t a
f <!> (Yield a s) = Yield a $ f s
_ <!> Done        = Done

data Unfold s a
  -- | An 'Unfold s a' takes a public state 's' and produces values of type 'a'.
  = Unfold { state   :: s
           , stepper :: s -> Step s a
           }

data Exec a
  -- | An execution wraps an unfold and hides its private state.
  = forall u. Unfoldable u => Exec (u a)

execution :: s -> (s -> Step s a) -> Exec a
execution s f = Exec $ Unfold s f

instance Unfoldable Exec where
  unfold (Exec u) = Exec <!> unfold u

instance Monoid (Exec a) where
  mempty = Exec $ Unfold () (const Done)
  e1 `mappend` e2 =
    Exec (Unfold (Left e1) stepper')
    where
      stepper' (Left e1') =
        case unfold e1' of
          Done        -> stepper' (Right e2)
          Yield a e1'' -> Yield a (Left e1'')
      stepper' (Right e2') = Right <!> unfold e2'
