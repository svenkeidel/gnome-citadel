{-# LANGUAGE TemplateHaskell, RankNTypes, FlexibleContexts #-}
module Scheduler ( Scheduler
                 , empty
                 , next
                 , add
                 ) where

import Control.Lens ((%=),(.=))
import Control.Lens.Getter(use)
import Control.Lens.TH
import Control.Applicative hiding (empty)
import Control.Monad.State hiding (state)

import qualified Unfold as U

newtype Scheduler u a =
  Scheduler
  { _unfolds :: [u a]
  }
makeLenses ''Scheduler

instance Show (u a) => Show (Scheduler u a) where
  show = show . _unfolds

-- | creates an empty scheduler
empty :: Scheduler u a
empty = Scheduler []

-- | adds an unfold to the scheduler.
add :: MonadState (Scheduler u a) m => u a -> m ()
add u = unfolds %= (u:)

-- | unfolds all stored 'Unfold's and returns the produced values
-- as a list. The order in which the produced values of the different
-- unfolds are returned may change between different calls of this
-- function.
next :: (U.Unfoldable u, Functor m, MonadState (Scheduler u a) m) => m [a]
next = do
  (steps, unfolds') <- go [] [] <$> use unfolds
  unfolds .= unfolds'
  return steps
  where
    go as us' []     = (as, us')
    go as us' (u:us) =
      case U.next u of
           U.Yield a u' ->  go (a:as) (u':us') us
           U.Done       ->  go    as      us'  us
