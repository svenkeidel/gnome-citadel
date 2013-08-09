module SequenceSpec(main,spec) where

import Test.Hspec
import qualified Data.Sequence as S
import Data.Sequence ( ViewL((:<)), ViewR ((:>)) )

main :: IO ()
main = hspec spec

exampleSeq :: S.Seq Int
exampleSeq = S.fromList [1..10]

spec :: Spec
spec = describe "A Sequence" $ do
  it "can be queried for the left head" $ do
    let x :< _ = S.viewl exampleSeq
    x `shouldBe` 1

  it "can be queried for the right head" $ do
    let _ :> x = S.viewr exampleSeq
    x `shouldBe` 10
