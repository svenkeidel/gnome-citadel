{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
module UnfoldSpec(main, spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Applicative
import Data.Monoid

import Unfold

main :: IO ()
main = hspec spec

instance Arbitrary a => Arbitrary (Unfold a) where
  arbitrary = unfold <$> (vector 10 :: Gen [a]) <*> pure unfoldList
  shrink    = fmap (\l -> unfold l unfoldList) . shrink . toList

spec :: Spec
spec = do
  describe "An Unfold" $ do
    it "can be unfolded step by step" $ do
      let u0 = unfold [1..10 :: Int] unfoldList
          Yield a1 u1 = next u0
      a1 `shouldBe` 1
      let Yield a2 _ = next u1
      a2 `shouldBe` 2

    context "when used as a Functor" $ do

      it "mappes the values correctly" $
        property $ \(l :: [Int]) -> toList (fmap (*2) (unfold l unfoldList)) == [2*x | x <- l]

      it "preserves identity" $
        property $ \(u :: Unfold Int) -> (fmap id u) == u

      it "is a homomorphism" $
        property $ \(i :: Int) (j :: Int) (u :: Unfold Int) ->
          let f = (*i)
              g = (+j)
          in fmap (f . g) u == fmap f (fmap g u)

    context "when used as an Applicative" $ do

      it "mappes the values correctly" $
        property $ \(l :: [Int]) ->
          let u = unfold l unfoldList
          in toList ((*) <$> pure 2 <*> u) == [2*x | x <- l]

      it "preserves identity" $
        property $ \(u :: Unfold Int) -> toList (pure id <*> u) == toList u

      it "is closed under composition" $
        property $ \(i :: Int) (j :: Int) (u :: Unfold Int) ->
          let f = pure (*i)
              g = pure (+j)
          in (pure (.) <*> f <*> g <*> u) == (f <*> (g <*> u))
      
      it "is an homomorphism" $
        property $ \(i :: Int) (x :: Int) ->
          let f = (*i)
          in (pure f <*> pure x :: Unfold Int) == (pure (f x))

      it "can interchange function and argument" $
        property $ \(i :: Int) (x :: Int) ->
          let f = pure (*i)
          in (f <*> pure x :: Unfold Int) == (pure ($ x) <*> f)

    context "when used as a monad" $ do

      it "binds correctly" $
        property $ \(l :: [Int]) ->
          let u = unfold l unfoldList
          in toList (u >>= (\a -> return $ 2 * a)) == [2*x | x <- l]

      it "preserves left application" $
        property $ \(i :: Int) (x :: Int) ->
          let f = return . (*i)
          in (return x >>= f :: Unfold Int) == f x

      it "preserves right identity" $
        property $ \(u :: Unfold Int) ->
          (u >>= return) == u
      
      it "preserves associativity" $
        property $ \(i :: Int) (j :: Int) (u :: Unfold Int) ->
          let f = return . (*i)
              g = return . (+j)
          in (u >>= (\x -> f x >>= g)) == ((u >>= f) >>= g)

    context "when used as a Monoid" $ do
      it "appends the values correctly" $
        property $ \(l :: [Int]) (k :: [Int]) ->
          let list l' = unfold l' unfoldList
          in toList (list l `mappend` list k) == l `mappend` k
      
      it "preserves right identity" $
        property $ \(u :: Unfold Int) ->
          u `mappend` mempty == u

      it "preserves left identity" $
        property $ \(u :: Unfold Int) ->
          mempty `mappend` u == u

      it "preserves associativity" $
        property $ \(u :: Unfold Int) (v :: Unfold Int) (w :: Unfold Int)->
          (u `mappend` v) `mappend` w == u `mappend` (v `mappend` w)
