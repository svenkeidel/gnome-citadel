{-# LANGUAGE TemplateHaskell, RankNTypes, FlexibleContexts, ExistentialQuantification #-}
module Scheduler ( execution
                 , scheduler
                 , nextSteps
                 , Step(..)
                 ) where

import Control.Lens ((%=),(.=))
import Control.Lens.Getter(use)
import Control.Lens.TH
import Control.Applicative
import Control.Monad.State hiding (state)


data Unfold p s a
  -- | An 'Unfold p s a' takes a public state 's', a private state 'p'
  -- | and produces values of type 'a'.
  = Unfold { state   :: p
           , stepper :: s -> p -> Step a p
           }

class Unfoldable u where
  unfold :: s -> u s a -> Step a (u s a)

instance Unfoldable (Unfold p) where
  unfold s u =
    case (stepper u) s (state u) of
      Yield a p -> Yield a $ u { state = p }
      Done      -> Done

data Step a s
  = Yield a s
  | Done

instance Functor (Step a) where
  fmap f (Yield a s) = Yield a $ f s
  fmap _ Done        = Done

data Exec s a
  -- | An execution wraps an unfold and hides its private state.
  = forall u. Unfoldable u => Exec (u s a)

instance Unfoldable Exec where
  unfold s (Exec u) = Exec <$> unfold s u

newtype Scheduler s a =
  Scheduler
  { _execs :: [Exec s a]
  }
makeLenses ''Scheduler

scheduler :: Scheduler s a
scheduler = Scheduler []

execution :: MonadState (Scheduler s a) m => p -> (s -> p -> Step a p) -> m ()
execution p f = execs %= (Exec (Unfold p f) :)

nextSteps :: (Functor m, MonadState (Scheduler s a) m)
         => s -> m [a]
nextSteps s = do
  (steps, execs') <- go [] [] <$> use execs
  execs .= execs'
  return steps
  where
    go as es' []     = (as, es')
    go as es' (e:es) =
      case unfold s e of
           Yield a e' ->  go (a:as) (e':es') es
           Done       ->  go    as      es'  es

