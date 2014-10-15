{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}
module Counter ( Counter
               , Identifier(..)
               , HasIdentifier(..)
               , Symbol(..)
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

class Symbol a where
      sym :: proxy a -> String

newtype Identifier a = Identifier Int
  deriving (Eq, Ord, Enum)

instance Symbol a => Show (Identifier a) where
  show i@(Identifier x) = sym i ++ " " ++ show x

instance NFData (Identifier a) where
  rnf (Identifier x) = rnf x

newtype Counter = Counter Int

freshId :: Counter -> (Identifier a, Counter)
freshId (Counter n) = (Identifier $ n+1, Counter $ n + 1)

instance Default Counter where
  def = Counter 0

asIdentifierOf :: Identifier a -> Identifier b
asIdentifierOf (Identifier i) = Identifier i
