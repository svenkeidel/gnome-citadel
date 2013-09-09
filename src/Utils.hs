module Utils ( runInState
             ) where

import Control.Monad.State
import Control.Monad.Reader

runInState :: (MonadState s ms) => Reader s a -> ms a
runInState r = do
  env <- get
  return $ runReader r env
