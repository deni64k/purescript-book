module Data.Path
  ( Path(..)
  , root
  , ls
  , filename
  , isDirectory
  , size
  , onlyFiles
  , whereIs
  , largestSmallest
  ) where

import Prelude

import Control.Alternative (guard)
import Control.Bind (join)
import Data.Array (catMaybes, concatMap, foldl, filter, head, init, last, uncons)
import Data.Ord (max, min)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.String.Common (split, joinWith)
import Data.String.Pattern

data Path
  = Directory String (Array Path)
  | File String Int

instance eqPath :: Eq Path where
  eq (File _ sz0) (File _ sz1) = sz0 == sz1
  eq _ _ = true

instance ordPath :: Ord Path where
  compare (File _ sz0) (File _ sz1) = compare sz0 sz1
  compare _ _ = EQ

instance showPath :: Show Path where
  show = filename

root :: Path
root =
  Directory "/"
    [ Directory "/bin/"
        [ File "/bin/cp" 24800
        , File "/bin/ls" 34700
        , File "/bin/mv" 20200
        ]
    , Directory "/etc/"
        [ File "/etc/hosts" 300
        ]
    , Directory "/home/"
        [ Directory "/home/user/"
            [ File "/home/user/todo.txt" 1020
            , Directory "/home/user/code/"
                [ Directory "/home/user/code/js/"
                    [ File "/home/user/code/js/test.js" 40000
                    ]
                , Directory "/home/user/code/haskell/"
                    [ File "/home/user/code/haskell/test.hs" 5000
                    ]
                ]
            ]
        ]
    ]

filename :: Path -> String
filename (File name _) = name
filename (Directory name _) = name

isDirectory :: Path -> Boolean
isDirectory (Directory _ _) = true
isDirectory _ = false

ls :: Path -> Array Path
ls (Directory _ xs) = xs
ls _ = []

size :: Path -> Maybe Int
size (File _ bytes) = Just bytes
size _ = Nothing

onlyFiles :: Path -> Array Path
onlyFiles (Directory _ xs) = concatMap onlyFiles xs
onlyFiles f = [f]

whereIs :: Path -> String -> Maybe Path
whereIs (File _ _) _ = Nothing
whereIs dir@(Directory _ files) fname =
  head <<< catMaybes $ map (whereIs' dir) files
  where
    whereIs' :: Path -> Path -> Maybe Path
    whereIs' dir' (File name _) = do
      guard $ name == filename dir' <> fname
      pure dir'
    whereIs' _ d@(Directory _ _) = whereIs d fname
  
largestSmallest :: Path -> Array Path
largestSmallest = rec <<< onlyFiles
  where
    rec :: Array Path -> Array Path
    rec files = case uncons files of
                Just { head: x, tail: [] } -> [ x ]
                Just { head: x, tail: xs } -> [ foldl min x xs, foldl max x xs ]
                _ -> []
