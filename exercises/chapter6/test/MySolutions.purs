module Test.MySolutions where

import Prelude

import Data.Array (nub, nubEq, nubByEq, length)
import Data.Eq.Generic (genericEq)
import Data.Foldable (class Foldable, foldl, foldr, foldMap, maximum)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Hashable (class Hashable, HashCode(..), hash, hashEqual)
import Data.Maybe
import Data.Newtype (class Newtype, over2, wrap)
import Data.Ord
import Data.Show.Generic (genericShow)

-- Note to reader: Add your solutions to this file

newtype Point
  = Point
  { x :: Number
  , y :: Number
  }

derive instance eqPoint :: Eq Point
derive instance ordPoint :: Ord Point

instance showPoint :: Show Point where
  show (Point p) =
    let x_ = show p.x
        y_ = show p.y
    in "(" <> x_ <> ", " <> y_ <> ")"

-- Complex

newtype Complex
  = Complex
  { real :: Number
  , imaginary :: Number
  }

instance showComplex :: Show Complex where
  show (Complex c) =
    let r = show c.real
        i = show $ abs c.imaginary
        sign = if c.imaginary < 0.0 then "-" else "+"
    in r <> sign <> i <> "i"

derive instance eqComplex :: Eq Complex

derive instance newtypeComplex :: Newtype Complex _

instance semiringComplex :: Semiring Complex where
  add = over2 wrap (+)
  mul = let mul' { real: r1, imaginary: i1} { real: r2, imaginary: i2} =
              { real: r1 * r2 - i1 * i2
              , imaginary: r1 * i2 + i1 * r2
              }
        in over2 wrap mul'
  zero = wrap zero
  one = wrap { real: one, imaginary: zero }

derive newtype instance ringComplex :: Ring Complex

-- Shape

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

derive instance eqShape :: Eq Shape
derive instance ordShape :: Ord Shape
derive instance genericShape :: Generic Shape _

instance showShape :: Show Shape where
  show = genericShow

dedupShapes :: Array Shape -> Array Shape
dedupShapes = nubEq

dedupShapesFast :: Array Shape -> Array Shape
dedupShapesFast = nub

-- NonEmpty

data NonEmpty a = NonEmpty a (Array a)

derive instance genericNonEmpty :: Generic (NonEmpty a) _

instance showNonEmpty :: Show a => Show (NonEmpty a) where
  show = genericShow

instance eqNonEmpty :: Eq a => Eq (NonEmpty a) where
  eq = genericEq

instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty x xs) (NonEmpty y ys) = NonEmpty x $ xs <> [ y ] <> ys

instance functorNonEmpty :: Functor NonEmpty where
  map f (NonEmpty x xs) = NonEmpty (f x) (f <$> xs)

instance foldableNonEmpty :: Foldable NonEmpty where
  foldr f init (NonEmpty x xs) = f x $ foldr f init xs
  foldl f init (NonEmpty x xs) = foldl f (f init x) xs
  foldMap f (NonEmpty x xs) = f x <> foldMap f xs

-- OneMore

data OneMore f a = OneMore a (f a)

instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
  foldr fn init (OneMore x xs) = fn x (foldr fn init xs)
  foldl fn init (OneMore x xs) = foldl fn (fn init x) xs
  foldMap fn (OneMore x xs) = fn x <> foldMap fn xs

-- Infinite values

data Extended a = Infinite | Finite a

instance eqExtended :: Eq a => Eq (Extended a) where
  eq (Finite x) (Finite y) = x == y
  eq Infinite Infinite = true
  eq _ _ = false

instance ordExtended :: Ord a => Ord (Extended a) where
  compare (Finite x) (Finite y) = compare x y
  compare Infinite (Finite _) = GT
  compare (Finite _) Infinite = LT
  compare Infinite Infinite = EQ

-- Partial

unsafeMaximum :: Partial => Array Int -> Int
unsafeMaximum xs = case maximum xs of
  Just mx -> mx

-- Action

class Monoid m <= Action m a where
  act :: m -> a -> a

newtype Multiply = Multiply Int

derive newtype instance eqMultiply :: Eq Multiply
derive newtype instance showMultiply :: Show Multiply

instance semigroupMultiply :: Semigroup Multiply where
  append (Multiply x) (Multiply y) = Multiply $ x*y

instance monoidMultiply :: Monoid Multiply where
  mempty = Multiply 1

instance actionMultiplyInt :: Action Multiply Int where
  act (Multiply m) x = m * x

instance actionMultiplyString :: Action Multiply String where
  act (Multiply m) x = repeat' mempty m
    where
      repeat' accum i | i < one   = accum
                      | otherwise = repeat' (accum <> x) (i-1)

instance actionMultiplyArray :: Action Multiply a => Action Multiply (Array a) where
  act m = map (\x -> act m x)

newtype Self m = Self m

derive newtype instance eqSelf :: Eq m => Eq (Self m)
derive newtype instance showSelf :: Show m => Show (Self m)

instance semigroupSelf :: Semigroup m => Semigroup (Self m) where
  append (Self x) (Self y) = Self $ x <> y

instance actionSelf :: Monoid a => Action a (Self a) where
  act m (Self x) = Self $ m <> x

-- Hashable

arrayHasDuplicates :: forall a. Hashable a => Array a -> Boolean
arrayHasDuplicates xs = not $ eq' xs $ nubByEq hashEqual' xs
  where eq' = eq `on` length
        hashEqual' x y = hashEqual x y && x == y

newtype Hour = Hour Int

instance eqHour :: Eq Hour where
  eq (Hour n) (Hour m) = mod n 12 == mod m 12

instance hashableHour :: Hashable Hour where
  hash (Hour h) = hash $ h `mod` 12
