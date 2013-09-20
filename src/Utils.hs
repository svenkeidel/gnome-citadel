module Utils ( runInState
             , (^->)
             ) where

import Control.Lens((^.))
import Control.Monad.State
import Control.Monad.Reader

import qualified Control.Lens.Getter as G

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
