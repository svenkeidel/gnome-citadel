module QueueSpec(main, spec) where

import Test.Hspec
import Queue
import Data.Maybe
import Data.Sequence as S

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "A queue" $ do
    let fortyTwo = 42 :: Int

    it "elements can be enqueued" $
      fromJust (peek $ enqueue fortyTwo S.empty) `shouldBe` fortyTwo

    it "elements can be dequeued" $
      fromJust (dequeue $ S.fromList [1,2,fortyTwo]) `shouldBe`
        (fortyTwo,S.fromList [1,2])
