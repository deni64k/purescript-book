module Test.MySolutions where

import Prelude

import Control.Alternative ((<|>))
import Control.Monad.Except.Trans (ExceptT, catchError, throwError)
import Control.Monad.Reader (Reader(..), runReader, ask, local)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.State (State(..), execState, modify)
import Control.Monad.State.Class (modify_)
import Control.Monad.State.Trans (StateT, get, put)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (Writer(..), execWriter, tell)
import Control.Monad.Writer.Trans (WriterT, runWriterT)
import Data.Array ((:), concat, intersperse, length, some, many)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, fold, foldl, foldM, traverse_)
import Data.Identity (Identity)
import Data.Int (even, odd)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (power)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.Number (nan)
import Data.String (stripPrefix)
import Data.String.CodeUnits (toCharArray)
import Data.String.Pattern (Pattern(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))

-- Note to reader : Add your solutions to this file

{-
import Prelude

import Data.Foldable (traverse_)
import Control.Monad.State
import Control.Monad.State.Class

sumArray :: Array Int -> State Int Unit
sumArray = traverse_ \n -> modify \sum -> sum + n

execState (sumArray [1, 2, 3] >>= \_ -> sumArray [4, 5] >>= \_ -> sumArray [6]) 0
runState  (sumArray [1, 2, 3] >>= \_ -> sumArray [4, 5] >>= \_ -> sumArray [6]) 0
evalState (sumArray [1, 2, 3] >>= \_ -> sumArray [4, 5] >>= \_ -> sumArray [6]) 0
-}

-- State

{-
testParens :: String -> Boolean
testParens =
  eq 0 <<< fromMaybe (-1) <<< foldM fn 0 <<< toCharArray
  where
    fn st c = case c of
      ')' -> if st < one then Nothing else Just $ st - one
      '(' -> Just $ st + one
      _   -> Just st
-}

testParens :: String -> Boolean
testParens parens =
  let
    fn :: Char -> Int -> Int
    fn '(' st | st >= zero = st + one
    fn ')' st              = st - one
    fn _   st              = st

    finalState :: State Int Unit
    finalState = traverse_ (modify_ <<< fn) $ toCharArray parens

    final :: Int
    final = execState finalState zero
  in
    final == zero

-- Reader

type Level = Int

type Doc = Reader Level String

spaces :: Int -> String
spaces = power " "

line :: String -> Doc
line s = do
  w <- ask
  pure $ spaces w <> s

indent :: Doc -> Doc
indent = local (\w -> w + 2)

cat :: Array Doc -> Doc
cat docs = sequence docs <#> intersperse "\n" <#> fold

render :: Doc -> String
render doc = runReader doc 0

-- Writer

sumArrayWriter :: forall f. Foldable f => f Int -> Writer (Additive Int) Unit
sumArrayWriter xs = do
  let sum = foldl (+) zero xs
  tell $ Additive sum

collatz :: Int -> Tuple Int (Array Int)
collatz n =
  let xs = execWriter $ collatz' n
  in (length xs - 1) /\ xs

collatz' :: Int -> Writer (Array Int) Unit
collatz' n | n == 1    = do
  tell [ 1 ]
  pure unit
collatz' n | even n    = do
  tell [ n ]
  collatz' (n / 2)
collatz' n | otherwise = do
  tell [ n ]
  collatz' (n * 3 + 1)

-- ExceptT

safeDivide :: forall a. Eq a => EuclideanRing a => a -> a -> ExceptT String Identity a
safeDivide x y
  | y == zero = do throwError "division by zero"
  | otherwise = do pure $ x / y

-- parser

type Errors = Array String

type Log = Array String

type Parser = StateT String (WriterT Log (ExceptT Errors Identity))

string :: String -> Parser String
string prefix = do
  st <- get
  tell [ "The state is " <> st ]
  case stripPrefix (Pattern prefix) st of
    Just s  -> do
      put s
      pure prefix
    Nothing -> throwError [ "Could not parse" ]

asFollowedByBs :: Parser String
asFollowedByBs = some (string "a") <> some (string "b") <#> fold

asOrBs :: Parser String
asOrBs = some (some (string "a") <|> some (string "b")) <#> concat <#> fold

-- indents with ReaderT and WriterT

type Parser' = ReaderT Level (WriterT Log Identity)

render' :: Parser' Unit -> String
render' m =
  let (_ /\ lines) = runRender' m
  in fold $ intersperse "\n" lines

runRender' :: Parser' Unit -> Tuple Unit Log
runRender' m = unwrap $ runWriterT $ runReaderT m 0

line' :: String -> Parser' Unit
line' s = do
  w <- ask
  let l = spaces w <> s
  tell [ l ]

indent' :: Parser' Unit -> Parser' Unit
indent' = do
  local (\w -> w + 2)
