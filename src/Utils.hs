module Utils ( runInState
             , (^->)
             , useFirst
             , fromRight
             , ($$)
             , applyAll
             , unlessM
             ) where

import           Control.Lens ((^.))
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Foldable (foldMap)
import           Data.Monoid (appEndo, Endo(Endo))

import qualified Control.Lens.Getter as G
import qualified Data.Foldable as DF
import qualified Data.Monoid as DM

runInState :: (MonadState s ms) => Reader s a -> ms a
runInState r = gets $ runReader r

-- | dereferences a 'method' of a type
-- useful when a field of a record takes the record itself as a first parameter:
--
-- @
-- data Level =
--   Level {
--     walkable :: Level -> Coord -> Bool
--     ...
--   }
-- @
--
-- >>> lvl ^-> walkable $ (1,3)
(^->) :: s -> G.Getting (s -> a) s (s -> a) -> a
s ^-> a = (s ^. a) s

useFirst:: DF.Foldable t => t (Maybe a) -> Maybe a
useFirst = DM.getFirst . DF.foldMap DM.First

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = error "Can not extract right value of Left"

infixr 0 $$
($$) :: DF.Foldable t => t (a -> a) -> a -> a
($$) fs = appEndo (foldMap Endo fs)

applyAll :: DF.Foldable t => t (a -> a) -> a -> a
applyAll = ($$)

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mb ma = do
  b <- mb
  if not b then ma else return ()
