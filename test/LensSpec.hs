{-# LANGUAGE TemplateHaskell #-}
module LensSpec(main,spec) where

import Control.Lens.TH
import Control.Lens
import Control.Monad.State (execState)

import Test.Hspec

data LevelWithQueue = L { _queue :: [Int] }
makeLenses ''LevelWithQueue

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Lenses" $ do
  let level = L [1,2,3]

  it "can be used to access and modify the example queue" $
    (queue %~ (5:) $ level) ^. queue `shouldBe` [5,1,2,3]

  it "can be used as state actions" $ do
    let newLevel = flip execState level $ do
          queue %= (++ [4])
          queue %= (0:)
          queue %= reverse
    newLevel ^. queue `shouldBe` [4,3,2,1,0]
