module Test.MySolutions where

import Prelude

import Control.Alternative (guard)
import Data.Array ((..), (:), snoc, concat, concatMap, head, tail, null, filter, foldl, foldr)
import Data.Int (quot)
import Data.Function (flip)
import Data.Maybe (fromMaybe, maybe)

-- Note to reader: Add your solutions to this file

isEven :: Int -> Boolean
isEven 0 = true
isEven 1 = false
isEven x | x < 0     = isEven $ -x
         | otherwise = isEven $ x-2

countEven :: Array Int -> Int
countEven [] = 0
countEven nums =
  let itis = maybe false isEven $ head nums
      rest = fromMaybe [] $ tail nums
  in (if itis then 1 else 0) + countEven rest

squared :: Array Number -> Array Number
squared xs = do
  x <- xs
  pure $ x*x

keepNonNegative :: Array Number -> Array Number
keepNonNegative = filter (_ >= 0.0)

infix 4 filter as <$?>
keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite = (<$?>) (0.0 <= _)

isPrime :: Int -> Boolean
isPrime 0 = false
isPrime 1 = false
isPrime x = null divisors
  where divisors = filter (_ /= x) >>> map (x `mod` _) >>> filter (_ == 0) $ (2 .. x)

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct xs ys = do
  x <- xs
  y <- ys
  pure [x, y]

triples :: Int -> Array (Array Int)
triples n = do
  i <- 1 .. n
  j <- i .. n
  k <- j .. n
  guard $ i*i + j*j == k*k
  pure [ i, j, k ]

factorize :: Int -> Array Int
factorize 1 = []
factorize origNum = factorizeRec [] 2 origNum
  where factorizeRec :: Array Int -> Int -> Int -> Array Int
        factorizeRec res i x | i == origNum   = pure i
                             | i >= x         = x:res
                             | x `mod` i == 0 = factorizeRec (i:res) (i+1) $ reduceDiv x i
                             | otherwise      = factorizeRec res (i+1) x
        reduceDiv :: Int -> Int -> Int
        reduceDiv 1 _ = 1
        reduceDiv x i | x `mod` i == 0 = reduceDiv (x `quot` i) i
                      | otherwise      = x

allTrue :: Array Boolean -> Boolean
allTrue = foldl (&&) true

fibTailRec :: Int -> Int
fibTailRec n = fibTailRecAcc 0 1 n
  where fibTailRecAcc x _ 0 = x
        fibTailRecAcc x y i = fibTailRecAcc y (x+y) (i-1)

reverse :: forall a. Array a -> Array a
reverse = foldr (flip snoc) []
