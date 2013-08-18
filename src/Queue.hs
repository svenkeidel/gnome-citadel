{-# LANGUAGE ViewPatterns #-}
module Queue ( Queue
             , fromList
             , enqueue
             , dequeue
             , peek
             ) where

import Prelude hiding (null)

import qualified Data.Sequence as S
import Data.Sequence ((<|), ViewR((:>)))

type Queue a = S.Seq a

fromList :: [a] -> Queue a
fromList = S.fromList

enqueue :: a -> Queue a -> Queue a
enqueue = (<|)

dequeue :: Queue a -> Maybe (a,Queue a)
dequeue (S.viewr -> xs :> x) = Just (x,xs)
dequeue _ = Nothing

peek :: Queue a -> Maybe a
peek (S.viewr -> _ :> x) = Just x
peek _ = Nothing
