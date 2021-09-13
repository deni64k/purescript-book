module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, nubByEq)
import Data.Maybe (Maybe, isJust)

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type AddressBook = List Entry

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <>
                   addr.city <> ", " <>
                   addr.state

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <>
                  entry.firstName <> ": " <>
                  showAddress entry.address

emptyBook :: AddressBook
emptyBook = empty

-- This line should have been automatically deleted by resetSolutions.sh. See Chapter 2 for instructions. NOTE TO MAINTAINER: If editing `insertEntry`, remember to also update the non-anchored (and unsimplified) version of this function that is hardcoded in the book text.
insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

-- This line should have been automatically deleted by resetSolutions.sh. See Chapter 2 for instructions. NOTE TO MAINTAINER: If editing `findEntry`, remember to also update the non-anchored (and unsimplified) version of this function that is hardcoded in the book text.
findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = filter f >>> head
  where f = _.address.street >>> eq street

isInBook :: String -> String -> AddressBook -> Boolean
isInBook f s = isJust <<< findEntry f s

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubByEq filterEntry
  where
  filterEntry :: Entry -> Entry -> Boolean
  filterEntry x y = x.firstName == y.firstName && x.lastName == y.lastName
