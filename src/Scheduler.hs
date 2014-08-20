{-# LANGUAGE TemplateHaskell, RankNTypes, FlexibleContexts #-}
module Scheduler ( Scheduler
                 , empty
                 , next
                 , add
                 ) where

import Control.Lens ((%%~),(%~))
import Control.Lens.TH

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
add :: u a -> Scheduler u a -> Scheduler u a
add u = unfolds %~ (u:)

-- | unfolds all stored 'Unfold's and returns the produced values
-- as a list. The order in which the produced values of the different
-- unfolds are returned may change between different calls of this
-- function.
next :: U.Unfoldable u => Scheduler u a -> ([a], Scheduler u a)
next = unfolds %%~ go [] []
  where
    go as us' []     = (as, us')
    go as us' (u:us) =
      case U.next u of
           U.Yield a u' -> go (a:as) (u':us') us
           U.Done       -> go    as      us'  us
