module HspecHelper ( levelShouldBe
                   ) where

import Control.Monad.Error (ErrorT)
import Control.Monad.Trans
import Test.Hspec (shouldBe)

import Level
import Level.Transformation

levelShouldBe :: [String] -> Level -> ErrorT LevelError IO Level
levelShouldBe s lvl = do
  lift $ show lvl `shouldBe` unlines s
  return lvl
