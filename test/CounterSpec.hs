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
      (n1:n2:n3:[]) <- replicateM 3 freshId
      lift $ n2 `shouldBe` n1 + 1
      lift $ n3 `shouldBe` n2 + 1
