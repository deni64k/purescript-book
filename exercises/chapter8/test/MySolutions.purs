module Test.MySolutions where

import Control.Plus (empty)
import Control.Monad.ST (ST, for, run)
import Control.Monad.ST.Ref (new, read, modify)
import Data.Array (head, tail, nub, sort, foldM)
import Data.List ((:), List(..))
import Data.Maybe
import Data.Int (toNumber)
import Effect (Effect)
import Effect.Exception (error, throwException)
import Math (pow)
import Prelude

-- Note to reader: Add your solutions to this file

third :: forall a. Array a -> Maybe a
third xs = do
  t1 <- tail xs
  t2 <- tail t1
  head t2

possibleSums :: Array Int -> Array Int
possibleSums = nub <<< sort <<< foldM (\st x -> [st, st + x]) 0

filterM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM _  Nil         = pure empty
filterM fn (Cons x xs) = do
  rest <- filterM fn xs
  cond <- fn x
  pure if cond
       then Cons x rest
       else rest

exceptionDivide :: forall a. Eq a => EuclideanRing a => a -> a -> Effect a
exceptionDivide x y =
  if y == zero
  then throwException $ error "div zero"
  else pure $ x / y

estimatePiSt :: forall r. Int -> ST r Number
estimatePiSt n = do
  st <- new 0.0
  for 1 n \k ->
    modify (gregoryTerm $ toNumber k) st
  final <- read st
  pure final
  where gregoryTerm k s = (pow (-1.0) (k + 1.0)) / (2.0 * k - 1.0) + s

estimatePi :: Int -> Number
estimatePi n = let
  pi4 = run (estimatePiSt n)
  in 4.0 * pi4

-- estimatePi n =
--   4.0 * run do
--     st <- new 0.0
--     for 1 n \k ->
--       modify (gregoryTerm $ toNumber k) st
--     read st
--   where gregoryTerm k s = (pow (-1.0) (k + 1.0)) / (2.0 * k - 1.0) + s

fibonacci :: Int -> Int
fibonacci n =
  run do
    st <- new {j: 0, k: 1}
    for 0 n \_ ->
      modify (\jk -> {j: jk.k, k: jk.j+jk.k}) st
    {j} <- read st
    pure j
