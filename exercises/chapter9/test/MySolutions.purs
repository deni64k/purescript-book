module Test.MySolutions where

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Control.Parallel (parTraverse, parOneOf)
import Data.Array ((:), concat)
import Data.Either (Either(..), hush)
import Data.Foldable (class Foldable, foldMap, foldl)
import Data.String (length)
import Data.String.Utils (lines)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (class Traversable, fold)
import Effect.Aff (Aff, Error, Milliseconds(..), attempt, delay, launchAff_)
-- import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.Path (FilePath)
import Node.Path as P
import Prelude

-- Note to reader: Add your solutions to this file

concatenateFiles :: FilePath -> FilePath -> FilePath -> Aff Unit
concatenateFiles src1 src2 dest = do
  src1Content <- readTextFile UTF8 src1
  src2Content <- readTextFile UTF8 src2
  writeTextFile UTF8 dest $ src1Content <> src2Content

concatenateMany :: forall m. Foldable m => m FilePath -> FilePath -> Aff Unit
concatenateMany srcFiles dest = do
  content <- foldMap (readTextFile UTF8) srcFiles
  writeTextFile UTF8 dest content

countCharacters :: FilePath -> Aff (Either Error Int)
countCharacters fname = do
  content <- attempt $ readTextFile UTF8 fname
  pure $ length <$> content

writeGet :: AX.URL -> FilePath -> Aff Unit
writeGet url dest = do
  resp <- AX.get ResponseFormat.string url
  case resp of
    Right { body } -> writeTextFile UTF8 dest body
    Left err       -> log $ "Error ocurred: " <> AX.printError err

concatenateManyParallel :: forall m. Traversable m => m FilePath -> FilePath -> Aff Unit
concatenateManyParallel srcFiles dest = do
  contents <- parTraverse (readTextFile UTF8) srcFiles
  writeTextFile UTF8 dest $ fold contents

getWithTimeout :: Number -> String -> Aff (Maybe String)
getWithTimeout timeout url =
  parOneOf
    [ do
         resp <- AX.get ResponseFormat.string url
         pure case resp of
           Right { body } -> Just body
           Left _         -> Nothing
    , do
         delay $ Milliseconds timeout
         pure Nothing
    ]

recurseFiles :: FilePath -> Aff (Array FilePath)
recurseFiles root = recurseFiles' (P.dirname root) (P.basename root)

recurseFiles' :: FilePath -> FilePath -> Aff (Array FilePath)
recurseFiles' curdir root = do
  let fullname = P.concat [ curdir, root ]
  rootContent <- map hush $ attempt $ readTextFile UTF8 fullname
  fromMaybe (pure mempty) do
    files <- rootContent <#> lines
    pure $ parTraverse (recurseFiles' $ P.dirname fullname) files
           <#> (\xs -> fullname : concat xs)
  -- case rootContent of
  --   Right content -> do
  --     subFiles <- foldl append mempty <$> parTraverse (recurseFiles' $ P.dirname fullname) $ lines content
  --     pure $ [ fullname ] <> subFiles
  --   Left _ -> pure mempty
