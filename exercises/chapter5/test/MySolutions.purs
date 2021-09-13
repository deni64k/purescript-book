module Test.MySolutions where

import Prelude

import ChapterExamples (Amp(..), Volt(..))
import Data.Person
import Data.Picture as P
import Data.Maybe
import Math (pi)

factorial :: Int -> Int
factorial n =
  factorial' n 1
  where
    factorial' :: Int -> Int -> Int
    factorial' 0 a = a
    factorial' i a = factorial' (i-1) (i*a)

binomial :: Int -> Int -> Int
binomial _ 0 = 1
binomial 0 _ = 0
binomial n k
  | n < k     = 0
  | n == k    = 1
  | otherwise = (factorial' n 1) / (factorial (n - k))
  where
    factorial' :: Int -> Int -> Int
    factorial' i a | i == 0    = a
                   | i == k    = a
                   | otherwise = factorial' (i-1) (i*a)

pascal :: Int -> Int -> Int
pascal _ 0 = 1
pascal 0 _ = 0
pascal n k = pascal (n-1) k + pascal (n-1) (k-1)

sameCity :: forall person1 address1. {address :: {city :: String | address1} | person1}
         -> forall person2 address2. {address :: {city :: String | address2} | person2}
         -> Boolean
sameCity {address: {city: c1}} {address: {city: c2}} = c1 == c2

livesInLA :: forall person1 address1. {address :: {city :: String | address1} | person1}
          -> Boolean
livesInLA {address: {city: "Los Angeles"}} = true
livesInLA _ = false

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [x] = x
fromSingleton d _ = d

circleAtOrigin :: P.Shape
circleAtOrigin = P.Circle P.origin 10.0

doubleScaleAndCenter :: P.Shape -> P.Shape
doubleScaleAndCenter (P.Circle _ r) = P.Circle P.origin (r*2.0)
doubleScaleAndCenter (P.Rectangle _ w h) = P.Rectangle P.origin (w*2.0) (h*2.0)
doubleScaleAndCenter (P.Text _ text) = P.Text P.origin text
doubleScaleAndCenter (P.Line start end) = P.Line start' end'
  where
    x' = (end.x - start.x)
    y' = (end.y - start.y)
    start' = {x: -x', y: -y'}
    end' = {x: x', y: y'}
doubleScaleAndCenter (P.Clipped _ _ _ _) = P.Line P.origin P.origin

shapeText (P.Text _ text) = Just text
shapeText _ = Nothing

newtype Watt = Watt Number

calculateWattage :: Amp -> Volt -> Watt
calculateWattage (Amp a) (Volt v) = Watt (a*v)

area :: P.Shape -> Number
area (P.Circle _ r) = pi * r*r
area (P.Rectangle _ w h) = w * h
area (P.Text _ _) = 0.0
area (P.Line _ _) = 0.0
area (P.Clipped _ _ _ _) = 0.0
