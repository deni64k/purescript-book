module Test.MySolutions where

import Control.Apply
import Data.AddressBook
import Data.AddressBook.Validation (Errors, matches, nonEmpty, validateAddress, validatePhoneNumbers)
import Data.Generic.Rep (class Generic)
import Data.Maybe
import Data.Foldable (class Foldable, foldr, foldl, foldMap)
import Data.Show.Generic (genericShow)
import Data.String.Regex (Regex)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (class Traversable, traverse, sequence)
import Data.Validation.Semigroup (V)
import Prelude

-- Note to reader: Add your solutions to this file

addMaybe :: forall a. Semiring a => Maybe a -> Maybe a -> Maybe a
addMaybe = lift2 (+)
subMaybe = lift2 (-)
mulMaybe = lift2 (*)
divMaybe = lift2 (/)

addApply :: forall m a. Apply m => Semiring a => m a -> m a -> m a
addApply = lift2 (+)
subApply = lift2 (-)
mulApply = lift2 (*)
divApply = lift2 (/)

combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe (Just xs) = pure <$> xs
combineMaybe Nothing = pure Nothing

-- Validation

stateRegex :: Regex
stateRegex = unsafeRegex "^[A-Za-z]{2}$" noFlags

nonEmptyRegex :: Regex
nonEmptyRegex = unsafeRegex "[^\\s]+" noFlags

validateAddressImproved :: Address -> V Errors Address
validateAddressImproved a =
  address <$> matches "Street" nonEmptyRegex a.street
          <*> matches "City" nonEmptyRegex a.city
          <*> matches "State" stateRegex a.state

-- Traversible

data Tree a = Leaf | Branch (Tree a) a (Tree a)

derive instance eqTree :: Eq a => Eq (Tree a)
derive instance genericTree :: Generic (Tree a) _

instance showTree :: Show a => Show (Tree a) where
  show t = genericShow t

instance functorTree :: Functor Tree where
  map fn (Branch l x r) = Branch (map fn l) (fn x) (map fn r)
  map _ Leaf = Leaf

instance foldableTree :: Foldable Tree where
  foldr _  st  Leaf          = st
  foldr fn st (Branch l x r) =
    let r' = foldr fn st r
        x' = fn x r'
    in foldr fn x' l

  foldl _  st  Leaf          = st
  foldl fn st (Branch l x r) =
    let l' = foldl fn st l
        x' = fn l' x
    in foldl fn x' r

  foldMap :: forall m a. Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Leaf            = mempty
  foldMap fn (Branch l x r) = foldMap fn l <> fn x <> foldMap fn r

instance traversableTree :: Traversable Tree where
  traverse :: forall m a b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
  traverse _  Leaf           = pure Leaf
  traverse fn (Branch l x r) = Branch <$> traverse fn l <*> fn x <*> traverse fn r

  sequence :: forall m a. Applicative m => Tree (m a) -> m (Tree a)
  sequence Leaf           = pure Leaf
  sequence (Branch l x r) = ado
    l' <- sequence l
    x' <- x
    r' <- sequence r
    in Branch l' x' r'

traversePreOrder :: forall m a b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePreOrder _  Leaf           = pure Leaf
traversePreOrder fn (Branch l x r) = ado
  x' <- fn x
  l' <- traversePreOrder fn l
  r' <- traversePreOrder fn r
  in Branch l' x' r'

traversePostOrder :: forall m a b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePostOrder _  Leaf           = pure Leaf
traversePostOrder fn (Branch l x r) = ado
  l' <- traversePostOrder fn l
  r' <- traversePostOrder fn r
  x' <- fn x
  in Branch l' x' r'

type PersonOptionalAddress
  = { firstName :: String
    , lastName :: String
    , homeAddress :: Maybe Address
    , phones :: Array PhoneNumber
    }

personOptionalAddress :: String -> String -> Maybe Address -> Array PhoneNumber -> PersonOptionalAddress
personOptionalAddress firstName lastName homeAddress phones = { firstName, lastName, homeAddress, phones }

validatePersonOptionalAddress :: PersonOptionalAddress -> V Errors PersonOptionalAddress
validatePersonOptionalAddress p =
  personOptionalAddress <$> nonEmpty "First Name" p.firstName
                        <*> nonEmpty "Last Name" p.lastName
                        <*> traverse validateAddress p.homeAddress
                        <*> validatePhoneNumbers "Phone Numbers" p.phones

sequenceUsingTraverse :: forall m f a. Traversable f => Applicative m => f (m a) -> m (f a)
sequenceUsingTraverse = traverse identity

traverseUsingSequence :: forall m f a b. Traversable f => Applicative m => (a -> m b) -> f a -> m (f b)
traverseUsingSequence fn = sequence <<< map fn
