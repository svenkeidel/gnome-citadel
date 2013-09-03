{-# LANGUAGE FlexibleContexts #-}
module ErrorTSpec(main, spec) where

import Control.Monad.State
import Prelude hiding (div)
import qualified Prelude as P
import Control.Monad.Error

import Test.Hspec

data ArithException = DivideByZero
  deriving (Show, Eq)

instance Error ArithException where
  noMsg = DivideByZero

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "An Error Monad Transformer" $ do

  let div :: (Integral a, MonadError ArithException m) => a -> a -> m a 
      _ `div` 0 = throwError DivideByZero
      x `div` y = return $ x `P.div` y
      divBy :: (Integral a, MonadState a m, MonadError ArithException m) => a -> m ()
      divBy 0 = throwError DivideByZero
      divBy y = modify (\x -> x `P.div` y)

  it "can throw errors" $ do
    runErrorT ((4 :: Int) `div` 2) `shouldReturn` Right 2
    runErrorT ((4 :: Int) `div` 0) `shouldReturn` Left DivideByZero

  it "can be combined with the state monad" $ do
    (runErrorT $ flip execStateT (4 :: Int) $ divBy 2) `shouldReturn` Right 2
    (runErrorT $ flip execStateT (4 :: Int) $ divBy 0) `shouldReturn` Left DivideByZero
    (runErrorT $ flip execStateT (4 :: Int) $ divBy 2 >> divBy 0) `shouldReturn` Left DivideByZero
    (runErrorT $ flip execStateT (4 :: Int) $ divBy 0 >> divBy 2) `shouldReturn` Left DivideByZero

  it "has a different semantic if it is deconstructed in opposite order" $ do
    (flip execStateT (4 :: Int) $ runErrorT $ divBy 2) `shouldReturn` 2
    (flip execStateT (4 :: Int) $ runErrorT $ divBy 0) `shouldReturn` 4
    (flip execStateT (4 :: Int) $ runErrorT $ divBy 2 >> divBy 0) `shouldReturn` 2
    (flip execStateT (4 :: Int) $ runErrorT $ divBy 0 >> divBy 2) `shouldReturn` 4

  it "can be handled if an error did occured" $ do
    let ignoreDivByZero = const $ return ()
    (runErrorT $ flip execStateT (4 :: Int) $ divBy 0 `catchError` ignoreDivByZero >> divBy 2)
       `shouldReturn` Right 2
