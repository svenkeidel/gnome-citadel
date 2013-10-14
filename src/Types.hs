module Types ( Identifier
             , TaskType(..)
             ) where

type Identifier a = Int

data TaskType = Mine | Lumber deriving (Show,Eq,Ord)
