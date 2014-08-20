module Counter ( Counter
               , Identifier(..)
               , freshId
               , asIdentifierOf
               ) where

import Data.Default

newtype Identifier a = Identifier Int
  deriving (Eq, Ord, Show)

newtype Counter = Counter Int

freshId :: Counter -> (Identifier a, Counter)
freshId (Counter n) = (Identifier $ n+1, Counter $ n + 1)

instance Default Counter where
  def = Counter 0

asIdentifierOf :: Identifier a -> Identifier b
asIdentifierOf (Identifier i) = Identifier i
