{-# LANGUAGE TemplateHaskell #-}
module LensSpec(main,spec) where

import Control.Lens.TH
import Control.Lens
import Control.Monad.State (execState)
import Data.Functor.Compose

import Test.Hspec

data LevelWithQueue = L { _queue :: [Int], _lId :: Int }
makeLenses ''LevelWithQueue

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Lenses" $ do
  let level = L [1,2,3] 0

  it "can be used to access and modify the example queue" $
    (queue %~ (5:) $ level) ^. queue `shouldBe` [5,1,2,3]

  it "can be used as state actions" $ do
    let newLevel = flip execState level $ do
          queue %= (++ [4])
          queue %= (0:)
          queue %= reverse
    newLevel ^. queue `shouldBe` [4,3,2,1,0]

  it "can return the new/old value alongside when setting" $ do
    let (oldId, newLevel) = lId <<%~ (+1) $ level
    let (newId, _) = lId <%~ (+1) $ level
    oldId `shouldBe` 0
    newId `shouldBe` 1
    newLevel ^. lId `shouldBe` 1

  context "isomorphisms" $ do

    it "can be used with Data.Functor.Compose" $ do
      let compose :: Iso' (f (g a)) (Compose f g a)
          compose = iso Compose getCompose
          input =    [ [1,2,3]
                     , [4,5,6]
                     , [7,8,9]] :: [[Int]]
          expected = [ [2,3,4]
                     , [5,6,7]
                     , [8,9,10]] :: [[Int]]
      over compose (fmap (+1)) input `shouldBe` expected
