module CounterSpec(main, spec) where

import Control.Monad
import Control.Monad.State

import Data.Default

import Test.Hspec

import Counter

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "A Counter" $ do

  it "can generate fresh identifiers" $ do
    void $ flip runStateT (def :: Counter) $ do
      (n1:n2:n3:[]) <- replicateM 3 (state freshId)
      lift $ (n2 /= n1 && n3 /= n2 && n3 /= n1) `shouldBe` True
