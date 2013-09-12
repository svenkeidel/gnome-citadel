{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, FlexibleInstances #-}
module UnfoldSpec(main, spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Applicative
import Data.Monoid
import Data.Functor.Identity
import Data.Function(on)

import Unfold

main :: IO ()
main = hspec spec

instance Arbitrary a => Arbitrary (Unfold a) where
  arbitrary = unfold <$> (vector 10 :: Gen [a]) <*> pure unfoldList
  shrink    = fmap (\l -> unfold l unfoldList) . shrink . toList

instance (Arbitrary a) => Arbitrary (UnfoldT Identity a) where
  arbitrary = (UnfoldT . return) <$> arbitrary
  shrink    = fmap (UnfoldT . Identity) . shrink . runIdentity . runUnfoldT

instance Show a => Show (UnfoldT Identity a) where
  show = show . runIdentity . runUnfoldT

instance Eq a => Eq (UnfoldT Identity a) where
  (==) = (==) `on` (runIdentity . runUnfoldT)

spec :: Spec
spec = do
  describe "An Unfold" $ do
    it "can be unfolded step by step" $ do
      let u0 = unfold [1..10 :: Int] unfoldList
          Yield a1 u1 = next u0
      a1 `shouldBe` 1
      let Yield a2 _ = next u1
      a2 `shouldBe` 2

    functorProperty fromList toList
    applicativeProperty fromList toList
    monadProperty fromList toList
    monoidProperty fromList toList

  describe "An UnfoldT" $ do

    let fromListT = UnfoldT . Identity . fromList
        toListT   = toList . runIdentity . runUnfoldT

    functorProperty fromListT toListT
    applicativeProperty fromListT toListT
    monadProperty fromListT toListT
    monoidProperty fromListT toListT


functorProperty :: forall f. (Show (f Int), Eq (f Int), Functor f, Arbitrary (f Int))
                => (forall a. [a] -> f a) -> (forall a. f a -> [a]) -> Spec
functorProperty fromList' toList' =
  context "when used as a Functor" $ do

    it "mappes the values correctly" $
      property $ \(l :: [Int]) -> toList' (fmap (*2) (fromList' l)) == [2*x | x <- l]

    it "preserves identity" $
      property $ \(u :: f Int) -> (fmap id u) == u

    it "is a homomorphism" $
      property $ \(i :: Int) (j :: Int) (u :: f Int) ->
      let f = (*i)
          g = (+j)
      in fmap (f . g) u == fmap f (fmap g u)


applicativeProperty :: forall f. (Show (f Int), Eq (f Int), Applicative f, Arbitrary (f Int))
                    => (forall a. [a] -> f a) -> (forall a. f a -> [a]) -> Spec
applicativeProperty fromList' toList' =
  context "when used as an Applicative" $ do

    it "mappes the values correctly" $
      property $ \(l :: [Int]) ->
      let u = fromList' l
      in toList' ((*) <$> pure 2 <*> u) == [2*x | x <- l]

    it "preserves identity" $
      property $ \(u :: f Int) -> toList' (pure id <*> u) == toList' u

    it "is closed under composition" $
      property $ \(i :: Int) (j :: Int) (u :: f Int) ->
      let f = pure (*i)
          g = pure (+j)
      in (pure (.) <*> f <*> g <*> u) == (f <*> (g <*> u))
      
    it "is an homomorphism" $
      property $ \(i :: Int) (x :: Int) ->
      let f = (*i)
      in (pure f <*> pure x :: f Int) == (pure (f x))

    it "can interchange function and argument" $
      property $ \(i :: Int) (x :: Int) ->
      let f = pure (*i)
      in (f <*> pure x :: f Int) == (pure ($ x) <*> f)

monadProperty :: forall f. (Show (f Int), Eq (f Int), Monad f, Arbitrary (f Int))
                    => (forall a. [a] -> f a) -> (forall a. f a -> [a]) -> Spec
monadProperty fromList' toList' =
  context "when used as a monad" $ do

    it "binds correctly" $
      property $ \(l :: [Int]) ->
        let u = fromList' l
        in toList' (u >>= (\a -> return $ 2 * a)) == [2*x | x <- l]

    it "preserves left application" $
      property $ \(i :: Int) (x :: Int) ->
        let f = return . (*i)
        in (return x >>= f :: f Int) == f x

    it "preserves right identity" $
      property $ \(u :: f Int) ->
        (u >>= return) == u
      
    it "preserves associativity" $
      property $ \(i :: Int) (j :: Int) (u :: f Int) ->
        let f = return . (*i)
            g = return . (+j)
        in (u >>= (\x -> f x >>= g)) == ((u >>= f) >>= g)


monoidProperty :: forall f. (Show (f Int), Eq (f Int), Monoid (f Int), Arbitrary (f Int))
                    => (forall a. [a] -> f a) -> (forall a. f a -> [a]) -> Spec
monoidProperty fromList' toList' =
  context "when used as a Monoid" $ do
    it "appends the values correctly" $
      property $ \(l :: [Int]) (k :: [Int]) ->
        let list l' = fromList' l'
        in toList' (list l `mappend` list k) == l `mappend` k
      
    it "preserves right identity" $
      property $ \(u :: f Int) ->
        u `mappend` mempty == u

    it "preserves left identity" $
      property $ \(u :: f Int) ->
        mempty `mappend` u == u

    it "preserves associativity" $
      property $ \(u :: f Int) (v :: f Int) (w :: f Int)->
        (u `mappend` v) `mappend` w == u `mappend` (v `mappend` w)
