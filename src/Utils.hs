module Utils ( ifM
             ) where

ifM :: Monad m => m Bool -> a -> a -> m a
ifM p a b = do
  p' <- p
  if p'
    then return a
    else return b
