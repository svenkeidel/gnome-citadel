module Coroutines where

import Coroutine
import Control.Applicative

producer :: (Applicative m, Monad m) => Coroutine (Yield Int) m (Int -> Int)
producer = do yield 1
              yield 4
              return $ (+2)

producer2 :: (Applicative m, Monad m) => Coroutine (Yield Int) m Int
producer2 = do yield 42
               yield 15
               yield 18
               return 6

producer3 :: (Applicative m, Monad m) => Coroutine (Yield Int) m Int
producer3 = producer <*> producer2

add :: Coroutine (Await Int) IO String
add = do
  i <- await
  j <- await
  return $ show $ i + j

converter :: Coroutine (Pipe Int String) IO ()
converter = do
  pipe show
  pipe (show . (*4))
  pipe (reverse . show)

runPipe :: (Show r, Show a) => [t] -> r -> Coroutine (Pipe t a) IO r -> IO r
runPipe l r c = do
  z <- resume c
  case (l,z) of
    (x:xs,Left (Pipe f)) -> do
      let (z',c') = f x
      print z'
      runPipe xs r c'
    ([],Left _) -> return r
    (_, Right res) -> return res

printer :: (Show a, Show b) => Coroutine (Yield a) IO b -> IO ()
printer c = do
  z <- resume c
  case z of
    Left (Yield a c') -> do
      print a
      printer c'
    Right r ->
      print r
