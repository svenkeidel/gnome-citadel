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
  let four = 4 :: Int
      runStateT' = flip runStateT

      div :: (Integral a, MonadError ArithException m) => a -> a -> m a
      _ `div` 0 = throwError DivideByZero
      x `div` y = return $ x `P.div` y

      divBy :: (Integral a, MonadState a m, MonadError ArithException m) => a -> m ()
      divBy 0 = throwError DivideByZero
      divBy y = modify (`P.div` y)

  it "can throw errors" $ do
    runErrorT (four `div` 2) `shouldReturn` Right 2
    runErrorT (four `div` 0) `shouldReturn` Left DivideByZero

  context "running StateT before ErrorT" $
    it "returns the result and final state of the state monad or only an error" $ do
       runErrorT (runStateT' four $ divBy 2) `shouldReturn` Right ((),2)
       runErrorT (runStateT' four $ divBy 0) `shouldReturn` Left DivideByZero
       runErrorT (runStateT' four $ divBy 2 >> divBy 0) `shouldReturn` Left DivideByZero
       runErrorT (runStateT' four $ divBy 0 >> divBy 2) `shouldReturn` Left DivideByZero

  context "running ErrorT before StateT" $
    it "returns the result from the state action or an error, together with the last state" $ do
      runStateT (runErrorT $ divBy 2) four `shouldReturn` (Right (), 2)
      runStateT (runErrorT $ divBy 0) four `shouldReturn` (Left DivideByZero, 4)
      runStateT (runErrorT $ divBy 2 >> divBy 0) four `shouldReturn` (Left DivideByZero, 2)
      runStateT (runErrorT $ divBy 0 >> divBy 2) four `shouldReturn` (Left DivideByZero, 4)

  it "can be handled if an error did occur" $ do
    let ignoreDivByZero = const $ return ()
    runErrorT (runStateT' four $ divBy 0 `catchError` ignoreDivByZero >> divBy 2)
       `shouldReturn` Right ((),2)