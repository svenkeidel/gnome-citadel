{-# LANGUAGE TemplateHaskell, RankNTypes, FlexibleContexts, ExistentialQuantification #-}
module Scheduler ( execution
                 , Exec
                 , scheduler
                 , nextSteps
                 , Step(..)
                 ) where

import Control.Lens ((%=),(.=))
import Control.Lens.Getter(use)
import Control.Lens.TH
import Control.Applicative
import Control.Monad.State hiding (state)
import Data.Monoid

data Unfold s a
  -- | An 'Unfold s a' takes a public state 's' and produces values of type 'a'.
  = Unfold { state   :: s
           , stepper :: s -> Step s a
           }

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

data Exec a
  -- | An execution wraps an unfold and hides its private state.
  = forall u. Unfoldable u => Exec (u a)

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

newtype Scheduler a =
  Scheduler
  { _execs :: [Exec a]
  }
makeLenses ''Scheduler

scheduler :: Scheduler a
scheduler = Scheduler []

execution :: MonadState (Scheduler a) m => s -> (s -> Step s a) -> m ()
execution s f = execs %= (Exec (Unfold s f) :)

nextSteps :: (Functor m, MonadState (Scheduler a) m) => m [a]
nextSteps = do
  (steps, execs') <- go [] [] <$> use execs
  execs .= execs'
  return steps
  where
    go as es' []     = (as, es')
    go as es' (e:es) =
      case unfold e of
           Yield a e' ->  go (a:as) (e':es') es
           Done       ->  go    as      es'  es

