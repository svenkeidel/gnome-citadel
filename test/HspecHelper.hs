module HspecHelper ( levelShouldBe
                   ) where

import Control.Monad.Trans
import Test.Hspec (shouldBe)

import Level

levelShouldBe :: (Monad (m IO), MonadTrans m) => [String] -> Level -> m IO Level
levelShouldBe s lvl = do
  lift $ show lvl `shouldBe` unlines s
  return lvl
