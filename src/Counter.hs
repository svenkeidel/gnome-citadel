{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}
module Counter ( Counter
               , Identifier(..)
               , HasIdentifier(..)
               , freshId
               , asIdentifierOf
               ) where

import Data.Default
import Control.DeepSeq (NFData,rnf)

class HasIdentifier a where
  type Identifiable a
  getIdentifier :: a -> Identifier (Identifiable a)

instance HasIdentifier (Identifier a) where
  type Identifiable (Identifier a) = a
  getIdentifier (Identifier i) = (Identifier i)

newtype Identifier a = Identifier Int
  deriving (Eq, Ord, Show, Enum)

instance NFData (Identifier a) where
  rnf (Identifier x) = rnf x

newtype Counter = Counter Int

freshId :: Counter -> (Identifier a, Counter)
freshId (Counter n) = (Identifier $ n+1, Counter $ n + 1)

instance Default Counter where
  def = Counter 0

asIdentifierOf :: Identifier a -> Identifier b
asIdentifierOf (Identifier i) = Identifier i
