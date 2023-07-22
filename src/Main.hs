{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Redundant fmap" #-}
{-# HLINT ignore "Redundant pure" #-}
{-# HLINT ignore "Use forM_" #-}
{-# HLINT ignore "Use if" #-}

module Main (main) where

import Codec.Picture (
  Image (imageHeight, imageWidth),
  Pixel (pixelAt),
  PixelRGBA8 (PixelRGBA8),
  PngSavable (encodePngWithMetadata),
  convertRGBA8,
  readImageWithMetadata,
 )
import qualified Codec.Picture.Metadata as J
import Control.Applicative (Alternative (empty))
import Control.Exception (assert, evaluate)
import Control.Monad (foldM, msum, when, (<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List (foldl', intercalate, isPrefixOf, isSuffixOf, partition, sort, stripPrefix)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (takeBaseName, takeDirectory, takeExtension, (</>))
import System.IO (
  BufferMode (LineBuffering),
  IOMode (AppendMode),
  hPutStrLn,
  hSetBuffering,
  openFile,
 )

-- SD metadata format:
--
-- Aside from the positive prompt, all of the fields are keyed by an explicit string.
-- Some entries are comma separated, while others are line separated.
--
-- Metadatas {getMetadatas = [Unknown "parameters" :=> String "pixelart of a large glass building, high quality, masterpiece <lora:pixelart:1>\nNegative prompt: ugly, normal quality, low quality\nSteps: 40, Sampler: DPM++ 2M Karras, CFG scale: 7, Seed: 1162253888, Size: 624x696, Model hash: e4a30e4607, Model: majicmixRealistic_v6, Lora hashes: \"pixelart: cd4e9895fffa\", Version: v1.3.1-15-gd853bde0\nTemplate: pixelart of a large glass building, high quality, masterpiece <lora:pixelart:1>\nNegative Template: ugly, normal quality, low quality",Format :=> SourcePng,Width :=> 624,Height :=> 696]}

data Key
  = KeyPositivePrompt
  | KeyNegativePrompt
  | KeySteps
  | KeySampler
  | KeyCFGScale
  | KeySeed
  | KeyFaceRestoration
  | KeySize
  | KeyModelHash
  | KeyModel
  | KeyClipSkip
  | KeyCFGRescalePhi
  | KeyLoraHashes
  | KeyVersion
  | KeyTemplate
  | KeyNegativeTemplate
  | KeyControlNet0
  deriving (Bounded, Enum, Eq, Ord, Show)

keyToText :: Key -> String
keyToText key = case key of
  KeyPositivePrompt -> "Positive prompt"
  KeyNegativePrompt -> "Negative prompt"
  KeySteps -> "Steps"
  KeySampler -> "Sampler"
  KeyCFGScale -> "CFG scale"
  KeySeed -> "Seed"
  KeyFaceRestoration -> "Face restoration"
  KeySize -> "Size"
  KeyModelHash -> "Model hash"
  KeyModel -> "Model"
  KeyClipSkip -> "Clip skip"
  KeyCFGRescalePhi -> "CFG Rescale phi"
  KeyLoraHashes -> "Lora hashes"
  KeyVersion -> "Version"
  KeyTemplate -> "Template"
  KeyNegativeTemplate -> "Negative Template"
  KeyControlNet0 -> "ControlNet 0"

textToKey :: String -> Maybe Key
textToKey key = case key of
  (eq KeyPositivePrompt -> True) -> Just KeyPositivePrompt
  (eq KeyNegativePrompt -> True) -> Just KeyNegativePrompt
  (eq KeySteps -> True) -> Just KeySteps
  (eq KeySampler -> True) -> Just KeySampler
  (eq KeyCFGScale -> True) -> Just KeyCFGScale
  (eq KeySeed -> True) -> Just KeySeed
  (eq KeyFaceRestoration -> True) -> Just KeyFaceRestoration
  (eq KeySize -> True) -> Just KeySize
  (eq KeyModelHash -> True) -> Just KeyModelHash
  (eq KeyModel -> True) -> Just KeyModel
  (eq KeyClipSkip -> True) -> Just KeyClipSkip
  (eq KeyCFGRescalePhi -> True) -> Just KeyCFGRescalePhi
  (eq KeyLoraHashes -> True) -> Just KeyLoraHashes
  (eq KeyVersion -> True) -> Just KeyVersion
  (eq KeyTemplate -> True) -> Just KeyTemplate
  (eq KeyNegativeTemplate -> True) -> Just KeyNegativeTemplate
  (eq KeyControlNet0 -> True) -> Just KeyControlNet0
  _ -> Nothing
 where
  eq k s = keyToText k == s

data SDMetadata = SDMetadata
  { sdPositivePrompt :: (String, [SDExtension])
  , sdNegativePrompt :: (String, [SDExtension])
  , sdSteps :: Int
  , sdSampler :: String
  , sdCfgScale :: Int
  , sdSeed :: Int
  , sdFaceRestoration :: String
  , sdSize :: (Int, Int)
  , sdModelHash :: String
  , sdModel :: String
  , sdClipSkip :: Maybe Int
  , sdCfgRescalePhi :: Maybe Int
  , sdLoraHashes :: [(String, String)]
  , sdVersion :: String
  , sdPositiveTemplate :: (String, [SDExtension])
  , sdNegativeTemplate :: (String, [SDExtension])
  , sdControlNet0 :: Maybe String
  }
  deriving (Show)

emptySdMetadata :: SDMetadata
emptySdMetadata =
  SDMetadata
    { sdPositivePrompt = ("", [])
    , sdNegativePrompt = ("", [])
    , sdSteps = 0
    , sdSampler = ""
    , sdCfgScale = 0
    , sdSeed = 0
    , sdFaceRestoration = ""
    , sdSize = (0, 0)
    , sdModelHash = ""
    , sdModel = ""
    , sdClipSkip = Nothing
    , sdCfgRescalePhi = Nothing
    , sdLoraHashes = []
    , sdVersion = ""
    , sdPositiveTemplate = ("", [])
    , sdNegativeTemplate = ("", [])
    , sdControlNet0 = Nothing
    }

data SDMetadataEntry = SDMetadataEntry
  { sdKey :: String
  , sdValue :: String -- trimmed of leading and trailing whitespace
  }
  deriving (Show)

-- An SD extension is of the form `<ext:arg1:arg2:...:argN>`
-- where `ext` is the extension name and `arg1` through `argN` are the arguments, which may be empty.
data SDExtension = SDExtension
  { sdExtName :: String
  , sdExtArgs :: [String]
  }
  deriving (Show)

stripSdExtension :: String -> Maybe (String, SDExtension)
stripSdExtension = \case
  '<' : rest ->
    let (name, rest') = spanItem rest
     in go [name] rest'
  _ -> Nothing
 where
  spanItem = span (`notElem` (":>" :: String))

  go :: [String] -> String -> Maybe (String, SDExtension)
  go acc = \case
    ':' : argBegin ->
      let (arg, rest) = spanItem argBegin
       in go (arg : acc) rest
    '>' : rest ->
      let name : args = reverse $ map trim acc
       in Just (rest, SDExtension name args)
    _ -> Nothing

-- Removes the extension encoding from the input string and returns the extensions.
extractSdExtensions :: String -> (String, [SDExtension])
extractSdExtensions = go ("", [])
 where
  go (accText, accExts) = \case
    (stripSdExtension -> Just (rest, ext)) -> go (accText, ext : accExts) rest
    (c : rest) -> go (c : accText, accExts) rest
    [] -> (trimHeavy $ reverse accText, reverse accExts)

-- SD weighted text is of the form `(ipsum lorem foo bar:DECIMAL_WEIGHT)`
data SDWeightedText = SDWeightedText
  { sdText :: String
  , sdWeight :: String
  }
  deriving (Show)

-- Doesn't handle nested weighted text
stripSdWeightedText :: String -> Maybe (String, SDWeightedText)
stripSdWeightedText = \case
  '(' : rest ->
    let (text, rest') = span (`notElem` ":)") rest
     in go text "1.1" rest'
  _ -> Nothing
 where
  go :: String -> String -> String -> Maybe (String, SDWeightedText)
  go text weight = \case
    ':' : weightBegin ->
      let (weight', rest) = span (/= ')') weightBegin
          weight'' = trim weight'
       in case isDecimalNumber weight'' of
            True -> go text weight'' rest
            False -> Nothing
    ')' : rest -> Just (rest, SDWeightedText text weight)
    _ -> Nothing

-- Removes the weighted text encoding from the input string
removeTextWeights :: String -> String
removeTextWeights = go ""
 where
  go acc = \case
    (stripSdWeightedText -> Just (rest, weighted)) -> go (reverse (sdText weighted) ++ acc) rest
    (c : rest) -> go (c : acc) rest
    [] -> reverse acc

isDecimalNumber :: String -> Bool
isDecimalNumber ('-' : rest) = isDecimalNumber rest
isDecimalNumber s = case partition (== '.') s of
  ('.' : '.' : _, _) -> False
  (_, s') -> all (`elem` ['0' .. '9']) s'

trim :: String -> String
trim = f . f
 where
  f = reverse . dropWhile (== ' ')

-- trims front and tail and squishes multiple spaces into one
-- replaces all non-space whitespace with spaces
trimHeavy :: String -> String
trimHeavy = unwords . words

massageLines :: String -> String
massageLines = go
 where
  go = \case
    ' ' : '\n' : rest -> go $ '\n' : rest
    '\n' : ' ' : rest -> go $ '\n' : rest
    ' ' : ',' : rest -> go $ ',' : rest
    '\n' : ',' : rest -> ',' : go rest
    '\n' : '<' : rest -> '<' : go rest
    ',' : '\n' : rest -> ',' : ' ' : go rest
    c : rest -> c : go rest
    [] -> []

splitUnquotedCommas :: String -> [String]
splitUnquotedCommas = go [] "" False
 where
  go spine word inQuote = \case
    ',' : rest | not inQuote -> go (word : spine) "" False rest
    '"' : rest | inQuote -> go spine ('"' : word) False rest
    '"' : rest -> go spine ('"' : word) True rest
    c : rest -> go spine (c : word) inQuote rest
    [] -> reverse $ map reverse case word of
      "" -> spine
      _ -> word : spine

parseSdMetadata :: String -> SDMetadata
parseSdMetadata text = foldl' accumSdMetadata emptySdMetadata theLines
 where
  theLines = map trim case lines text of
    pos : neg : commaDelimJunk : rest -> pos : neg : splitUnquotedCommas commaDelimJunk ++ rest
    _ -> error "parseSdMetadata: not enough lines"

accumSdMetadata :: SDMetadata -> String -> SDMetadata
accumSdMetadata sd theLine = case theLine of
  (goEntry KeyNegativePrompt -> Just entry) -> sd{sdNegativePrompt = extractSdExtensions $ sdValue entry}
  (goEntry KeySteps -> Just entry) -> sd{sdSteps = read $ sdValue entry}
  (goEntry KeySampler -> Just entry) -> sd{sdSampler = sdValue entry}
  (goEntry KeyCFGScale -> Just entry) -> sd{sdCfgScale = read $ sdValue entry}
  (goEntry KeySeed -> Just entry) -> sd{sdSeed = read $ sdValue entry}
  (goEntry KeyFaceRestoration -> Just entry) -> sd{sdFaceRestoration = sdValue entry}
  (goEntry KeySize -> Just entry) -> sd{sdSize = readSize $ sdValue entry}
  (goEntry KeyModelHash -> Just entry) -> sd{sdModelHash = sdValue entry}
  (goEntry KeyModel -> Just entry) -> sd{sdModel = sdValue entry}
  (goEntry KeyClipSkip -> Just entry) -> sd{sdClipSkip = Just $ read $ sdValue entry}
  (goEntry KeyCFGRescalePhi -> Just entry) -> sd{sdCfgRescalePhi = Just $ read $ sdValue entry}
  (goEntry KeyLoraHashes -> Just entry) -> sd{sdLoraHashes = readLoraHashes $ sdValue entry}
  (goEntry KeyVersion -> Just entry) -> sd{sdVersion = sdValue entry}
  (goEntry KeyTemplate -> Just entry) -> sd{sdPositiveTemplate = extractSdExtensions $ sdValue entry}
  (goEntry KeyNegativeTemplate -> Just entry) -> sd{sdNegativeTemplate = extractSdExtensions $ sdValue entry}
  (goEntry KeyControlNet0 -> Just entry) -> sd{sdControlNet0 = Just $ sdValue entry}
  -- KeyPositivePrompt is not keyed by string, so we have to do this special case
  _ -> case sdPositivePrompt sd of
    ("", []) -> sd{sdPositivePrompt = extractSdExtensions theLine}
    _ -> error $ "parse error: " ++ show theLine ++ " is not a valid SD metadata entry"
 where
  goEntry = parseSdMetadataEntry . keyToText

parseSdMetadataEntry :: String -> String -> Maybe SDMetadataEntry
parseSdMetadataEntry key text = case stripPrefix key' text of
  Nothing -> Nothing
  Just text' -> Just $ SDMetadataEntry key $ trim text'
 where
  key' = key ++ ":"

readSize :: String -> (Int, Int)
readSize text = (read w, read h)
 where
  (w, _ : h) = break (== 'x') text

readLoraHashes :: String -> [(String, String)]
readLoraHashes text = map readLoraHash $ splitOn "," text

readLoraHash :: String -> (String, String)
readLoraHash text = (key, trim value)
 where
  (key, _ : value) = break (== ':') text

inlineSdExtensions :: (String, [SDExtension]) -> String
inlineSdExtensions (text, exts) = text ++ concatMap inlineSdExtension exts
 where
  inlineSdExtension (SDExtension name args) = " <" ++ name ++ concatMap (':' :) args ++ ">"

toParameters :: SDMetadata -> (J.Keys J.Value, J.Value)
toParameters sd = (J.Unknown "parameters", value)
 where
  showKey key str = Just $ keyToText key ++ ": " ++ str
  value =
    J.String $
      unlines $
        catMaybes
          [ Just $ inlineSdExtensions $ sdPositivePrompt sd
          , showKey KeyNegativePrompt $ inlineSdExtensions (sdNegativePrompt sd)
          , Just $ intercalate ", " commaDelimJunk
          , if null $ sdPositiveTemplate sd
              then Nothing
              else showKey KeyTemplate $ inlineSdExtensions (sdPositiveTemplate sd)
          , if null $ sdNegativeTemplate sd
              then Nothing
              else showKey KeyNegativeTemplate $ inlineSdExtensions (sdNegativeTemplate sd)
          ]
  commaDelimJunk =
    catMaybes
      [ showKey KeySteps $ show (sdSteps sd)
      , showKey KeySampler $ sdSampler sd
      , showKey KeyCFGScale $ show (sdCfgScale sd)
      , showKey KeySeed $ show (sdSeed sd)
      , case sdFaceRestoration sd of
          "" -> Nothing
          face -> showKey KeyFaceRestoration face
      , showKey KeySize $ show (fst $ sdSize sd) ++ "x" ++ show (snd $ sdSize sd)
      , showKey KeyModelHash $ sdModelHash sd
      , showKey KeyModel $ sdModel sd
      , case sdClipSkip sd of
          Nothing -> Nothing
          Just clipSkip -> showKey KeyClipSkip $ show clipSkip
      , case sdCfgRescalePhi sd of
          Nothing -> Nothing
          Just phi -> showKey KeyCFGRescalePhi $ show phi
      , showKey KeyLoraHashes $ intercalate "," (map (\(k, v) -> k ++ ": " ++ v) $ sdLoraHashes sd)
      , showKey KeyVersion $ sdVersion sd
      ]

prettySdMetadata :: Bool -> [Key] -> SDMetadata -> String
prettySdMetadata includeKey whitelist sd =
  intercalate "\n" $
    catMaybes
      [ showKey KeyPositivePrompt $ inlineSdExtensions (sdPositivePrompt sd)
      , showKey KeyNegativePrompt $ inlineSdExtensions (sdNegativePrompt sd)
      , showKey KeySteps $ show (sdSteps sd)
      , showKey KeySampler $ sdSampler sd
      , showKey KeyCFGScale $ show (sdCfgScale sd)
      , showKey KeySeed $ show (sdSeed sd)
      , case sdFaceRestoration sd of
          "" -> Nothing
          face -> showKey KeyFaceRestoration face
      , showKey KeySize $ show (fst $ sdSize sd) ++ "x" ++ show (snd $ sdSize sd)
      , showKey KeyModelHash $ sdModelHash sd
      , showKey KeyModel $ sdModel sd
      , case sdControlNet0 sd of
          Nothing -> Nothing
          Just controlNet0 -> showKey KeyControlNet0 controlNet0
      , case sdClipSkip sd of
          Nothing -> Nothing
          Just clipSkip -> showKey KeyClipSkip $ show clipSkip
      , case sdCfgRescalePhi sd of
          Nothing -> Nothing
          Just phi -> showKey KeyCFGRescalePhi $ show phi
      , showKey KeyLoraHashes $ intercalate "," (map (\(k, v) -> k ++ ": " ++ v) $ sdLoraHashes sd)
      , showKey KeyVersion $ sdVersion sd
      , if null $ sdPositiveTemplate sd
          then Nothing
          else showKey KeyTemplate $ inlineSdExtensions (sdPositiveTemplate sd)
      , if null $ sdNegativeTemplate sd
          then Nothing
          else showKey KeyNegativeTemplate $ inlineSdExtensions (sdNegativeTemplate sd)
      ]
 where
  showKey key str = case allowed key of
    True -> Just case includeKey of
      True -> keyToText key ++ ": " ++ str
      False -> str
    False -> Nothing
  allowed key = case whitelist of
    [] -> True
    _ -> key `elem` whitelist

applyToMetadatas :: SDMetadata -> J.Metadatas -> J.Metadatas
applyToMetadatas sd md =
  foldr
    ($)
    md
    [ uncurry J.insert (toParameters sd)
    , J.insert J.Width (fromIntegral $ fst $ sdSize sd)
    , J.insert J.Height (fromIntegral $ snd $ sdSize sd)
    , J.insert J.Format J.SourcePng
    ]

editSdMetadata :: (Key, String) -> SDMetadata -> SDMetadata
editSdMetadata (key, val) sd = case key of
  KeyPositivePrompt -> sd{sdPositivePrompt = extractSdExtensions val}
  KeyNegativePrompt -> sd{sdNegativePrompt = extractSdExtensions val}
  KeySteps -> sd{sdSteps = read val}
  KeySampler -> sd{sdSampler = val}
  KeyCFGScale -> sd{sdCfgScale = read val}
  KeySeed -> sd{sdSeed = read val}
  KeyFaceRestoration -> sd{sdFaceRestoration = val}
  KeySize -> sd{sdSize = readSize val}
  KeyModelHash -> sd{sdModelHash = val}
  KeyModel -> sd{sdModel = val}
  KeyClipSkip -> sd{sdClipSkip = Just $ read val}
  KeyCFGRescalePhi -> sd{sdCfgRescalePhi = Just $ read val}
  KeyLoraHashes -> sd{sdLoraHashes = readLoraHashes val}
  KeyVersion -> sd{sdVersion = val}
  KeyTemplate -> sd{sdPositiveTemplate = extractSdExtensions val}
  KeyNegativeTemplate -> sd{sdNegativeTemplate = extractSdExtensions val}
  KeyControlNet0 -> sd{sdControlNet0 = Just val}

removeMetadataPromptWeights :: SDMetadata -> SDMetadata
removeMetadataPromptWeights sd = sd'
 where
  (posPromptText, posPromptExts) = sdPositivePrompt sd
  (negPromptText, negPromptExts) = sdNegativePrompt sd
  posPromptText' = trim $ removeTextWeights posPromptText
  negPromptText' = trim $ removeTextWeights negPromptText
  sd' =
    sd
      { sdPositivePrompt = (posPromptText', posPromptExts)
      , sdNegativePrompt = (negPromptText', negPromptExts)
      }

data CLIOpts = CLIOpts
  { optHelp :: Bool
  , optInputImage :: FilePath
  , optOutputImage :: Maybe FilePath
  , optOutputCaption :: Maybe FilePath
  , optPrint :: Bool
  , optPrintKey :: [Key]
  , optLogFile :: Maybe FilePath
  , optPatternFile :: Maybe FilePath
  , optOverwriteInput :: Bool
  , optBatch :: Bool
  , optForce :: Bool
  , optCreateMissingDirs :: Bool
  , optEdit :: [(Key, String)]
  , optRemovePromptWeights :: Bool
  }
  deriving (Show)

emptyCliOpts :: CLIOpts
emptyCliOpts =
  CLIOpts
    { optHelp = False
    , optInputImage = ""
    , optOutputImage = Nothing
    , optOutputCaption = Nothing
    , optPrint = False
    , optPrintKey = []
    , optLogFile = Nothing
    , optPatternFile = Nothing
    , optOverwriteInput = False
    , optBatch = False
    , optForce = False
    , optCreateMissingDirs = False
    , optEdit = []
    , optRemovePromptWeights = False
    }

-- Usage:
--  sd-png-metadata <options>
--
-- Options
--   --help                Show this help text.
--   --input-image         Input image file path. Required.
--   --output-image        Output image file path.
--   --output-caption      Output caption file path.
--   --print               Print the metadata to stdout.
--   --print-key <key>     Print the metadata key to stdout.
--   --log-file            Log file path.
--   --pattern-file        Pattern file path for caption output.
--   --overwrite-input     Overwrite the input image with the metadata.
--   --batch               Batch mode. Specified outputs must be directories.
--   --force               Disable all overwrite checks.
--   --create-missing-dirs Create any missing directories.
--   --edit <key> <value>  Edit the metadata.
--   --no-prompt-weights   Removes weights from metadata prompts.
--
-- Pattern file format:
--   some text before subject * and some after
--   * some text after the subject
--   some text before the subject *
--   as many pattern lines * in the pattern file as you want
--
-- Supported keys: ****
--
helpMessage :: String
helpMessage =
  unlines
    [ "Usage:"
    , " sd-png-metadata <options>"
    , ""
    , "Options"
    , "  --help                Show this help text."
    , "  --input-image         Input image file path. Required."
    , "  --output-image        Output image file path."
    , "  --output-caption      Output caption file path."
    , "  --log-file            Log file path."
    , "  --pattern-file        Pattern file path for caption output."
    , "  --print               Print the input image metadata to stdout."
    , "  --print-key <key>     Print the input image metadata key to stdout."
    , "  --overwrite-input     Overwrite the input image with the metadata."
    , "  --batch               Batch mode. Specified files must be directories."
    , "  --force               Disable all overwrite checks."
    , "  --create-missing-dirs Create any missing directories."
    , "  --edit <key> <value>  Edit the metadata."
    , "  --no-prompt-weights   Removes weights from metadata prompts."
    , ""
    , "Pattern file format:"
    , "  some text before subject * and some after"
    , "  * some text after the subject"
    , "  some text before the subject *"
    , "  as many pattern lines * in the pattern file as you want"
    , ""
    , "Supported keys:" ++ intercalate "\n  " ("" : map showKey [minBound ..])
    ]
 where
  showKey key = "\"" ++ keyToText key ++ "\""

parseCliOpts :: [String] -> Either String CLIOpts
parseCliOpts = go' <=< go emptyCliOpts
 where
  go opts = \case
    "--help" : rest -> go opts{optHelp = True} rest
    "--input-image" : path : rest -> go opts{optInputImage = path} rest
    "--output-image" : path : rest -> go opts{optOutputImage = Just path} rest
    "--output-caption" : path : rest -> go opts{optOutputCaption = Just path} rest
    "--log-file" : path : rest -> go opts{optLogFile = Just path} rest
    "--pattern-file" : path : rest -> go opts{optPatternFile = Just path} rest
    "--print" : rest -> go opts{optPrint = True} rest
    "--print-key" : key : rest -> case toKey key of
      Right key' -> go opts{optPrintKey = key' : optPrintKey opts} rest
      Left err -> Left err
    "--overwrite-input" : rest -> go opts{optOverwriteInput = True} rest
    "--batch" : rest -> go opts{optBatch = True} rest
    "--force" : rest -> go opts{optForce = True} rest
    "--create-missing-dirs" : rest -> go opts{optCreateMissingDirs = True} rest
    "--edit" : key : value : rest -> case toKey key of
      Right key' -> go opts{optEdit = (key', value) : optEdit opts} rest
      Left err -> Left err
    "--no-prompt-weights" : rest -> go opts{optRemovePromptWeights = True} rest
    arg : rest -> Left $ "Unexpected argument: " ++ show (arg : rest)
    [] -> Right opts
  go' opts = do
    let validateInput o = case optInputImage o of
          "" -> Left "Missing input image path"
          _ -> Right o
    let handleOverwrite o = case optOverwriteInput o of
          False -> Right o
          True -> case optOutputImage o of
            Nothing -> Right o{optOutputImage = Just $ optInputImage o}
            Just _ -> Left "Cannot overwrite input image when output image is specified"
    let handleSpecialPrint o = case (optOutputImage o, optOutputCaption o, optBatch o, optPrintKey o) of
          (Nothing, Nothing, False, []) -> Right o{optPrint = True}
          _ -> Right o
    let fixPrintKeyOrder o = Right o{optPrintKey = reverse $ optPrintKey o}
    let fixups =
          [ validateInput
          , handleOverwrite
          , handleSpecialPrint
          , fixPrintKeyOrder
          ]
    if optHelp opts
      then pure opts
      else foldM (\o f -> f o) opts fixups
  toKey key = case textToKey key of
    Nothing -> Left $ "Unknown key: " ++ key
    Just key' -> Right key'

looksLikeDir :: FilePath -> Bool
looksLikeDir path = case reverse path of
  '/' : _ -> True
  '\\' : _ -> True
  _ -> False

validateBatch :: Either String CLIOpts -> IO (Either String CLIOpts)
validateBatch eOpts = do
  let validateInputImage o = case optInputImage o of
        path ->
          doesFileExist path >>= \case
            True -> pure $ Left "Batch requires input image to be a directory"
            False -> pure case looksLikeDir path of
              True -> Right o
              False -> Left "Batch requires input image directory to end with a slash"
  let validateOutputImage o = case optOutputImage o of
        Nothing -> pure $ Right o
        Just path ->
          doesFileExist path >>= \case
            True -> pure $ Left "Batch requires output image to be a directory"
            False -> pure case looksLikeDir path of
              True -> Right o
              False -> Left "Batch requires output image directory to end with a slash"
  let validateOutputCaption o = case optOutputCaption o of
        Nothing -> pure $ Right o
        Just path ->
          doesFileExist path >>= \case
            True -> pure $ Left "Batch requires output caption to be a directory"
            False -> pure case looksLikeDir path of
              True -> Right o
              False -> Left "Batch requires output caption directory to end with a slash"
  let checks =
        [ validateInputImage
        , validateOutputImage
        , validateOutputCaption
        ]
  let go eo f = case eo of
        Left err -> pure $ Left err
        Right o -> f o
  case eOpts of
    Left err -> pure $ Left err
    Right opts -> case optBatch opts of
      True -> foldM go eOpts checks
      False -> pure eOpts

forceEvalImage :: Image PixelRGBA8 -> IO ()
forceEvalImage image = do
  -- unable to use anything NFData-related because of dependency import problems
  let (w, h) = (imageWidth image, imageHeight image)
  let go x y
        | x >= w = go 0 (y + 1)
        | y >= h = pure ()
        | otherwise = do
          let PixelRGBA8 r g b a = pixelAt image x y
          _ <- evaluate r
          _ <- evaluate g
          _ <- evaluate b
          _ <- evaluate a
          go (x + 1) y
  go 0 0

forceEvalMetadatas :: J.Metadatas -> IO ()
forceEvalMetadatas metadatas = do
  -- unable to use anything NFData-related because of dependency import problems
  _ <- evaluate $ length $ show metadatas
  pure ()

patternWildcard :: Char
patternWildcard = '*'

mkPrefixPatterns :: [String] -> [String]
mkPrefixPatterns = map (takeWhile (/= patternWildcard))

mkSuffixPatterns :: [String] -> [String]
mkSuffixPatterns = map (tail . dropWhile (/= patternWildcard))

makePatternPairs :: [String] -> [(String, String)]
makePatternPairs patterns = zip (mkPrefixPatterns patterns) (mkSuffixPatterns patterns)

readPatternPairs :: FilePath -> IO [(String, String)]
readPatternPairs path = do
  contents <- readFile path
  let patterns = filter (not . null) $ map trim $ lines contents
  pure $ makePatternPairs patterns

extractSubject :: [(String, String)] -> String -> Maybe String
extractSubject [] str = Just str
extractSubject patternPairs str = msum do
  (prefix, suffix) <- patternPairs
  let isMatch = isPrefixOf prefix str && isSuffixOf suffix str
      subject = trim $ takeWhile (/= ',') $ drop (length prefix) (take (length str - length suffix) str)
  pure if isMatch then Just subject else Nothing

data DoSingle = DoSingle
  { oneInputImage :: FilePath
  , oneOutputImage :: Maybe FilePath
  , oneOutputCaption :: Maybe FilePath
  , oneLogger :: String -> IO ()
  , onePatternPairs :: [(String, String)]
  }

doSingle :: CLIOpts -> DoSingle -> MaybeT IO ()
doSingle opts single = do
  let inputFile = oneInputImage single
  let mOutputImage = oneOutputImage single
  let mOutputCaption = oneOutputCaption single
  let existsCheck path = case optForce opts of
        True -> pure ()
        False -> do
          exists <- liftIO $ doesFileExist path
          when exists do
            liftIO $ oneLogger single $ "Output file already exists: " ++ path
            empty
  liftIO (readImageWithMetadata inputFile) >>= \case
    Left err -> do
      liftIO $ oneLogger single $ "Error loading image: " ++ err
      empty
    Right (dynImage, metadatas) -> do
      let image = convertRGBA8 dynImage
      let parameters = J.lookup (J.Unknown "parameters") metadatas
      case parameters of
        Nothing -> do
          liftIO $ oneLogger single "No \"parameters\" metadata found"
          liftIO $ oneLogger single $ show metadatas
          empty
        Just (J.String text) -> do
          let text' = massageLines text
          let sd = parseSdMetadata text'
          let goWeights = case optRemovePromptWeights opts of
                True -> removeMetadataPromptWeights
                False -> id
          let sd' = goWeights $ foldr editSdMetadata sd (optEdit opts)
          let metadatas' = applyToMetadatas sd' metadatas
          when (optPrint opts) do
            liftIO $ putStrLn $ prettySdMetadata True [] sd'
          case optPrintKey opts of
            [] -> pure ()
            keys -> liftIO $ putStrLn $ prettySdMetadata False keys sd'
          liftIO $ forceEvalMetadatas metadatas' -- forces out error messages before we start writing files
          case mOutputImage of
            Nothing -> pure ()
            Just path -> do
              if optOverwriteInput opts
                then do
                  assert (path == inputFile) $ pure ()
                  -- XXX: a less silly way of doing this would be to load as a ByteString, evaluate that instead, and then decode the image and metadata
                  liftIO $ forceEvalImage image
                else do
                  existsCheck path
              let bs = encodePngWithMetadata metadatas' image
              liftIO $ BS.writeFile path bs
          case mOutputCaption of
            Nothing -> pure ()
            Just path -> do
              existsCheck path
              let caption = inlineSdExtensions $ sdPositivePrompt sd'
              caption' <- case extractSubject (onePatternPairs single) caption of
                Just subject -> pure subject
                Nothing -> do
                  liftIO $ oneLogger single $ "Unable to extract subject from : " ++ caption
                  -- pure caption
                  empty
              liftIO $ writeFile path caption'
        Just val -> do
          liftIO $ oneLogger single $ "Unexpected \"parameters\" metadata value: " ++ show val
          empty

isPng :: FilePath -> Bool
isPng path = ext == ".png" || ext == ".PNG"
 where
  ext = takeExtension path

type Logger = String -> IO ()

doBatch :: CLIOpts -> Logger -> [(String, String)] -> IO ()
doBatch opts logger patternPairs = do
  when (optCreateMissingDirs opts) do
    case optOutputImage opts of
      Nothing -> pure ()
      Just path -> createDirectoryIfMissing True $ takeDirectory path
    case optOutputCaption opts of
      Nothing -> pure ()
      Just path -> createDirectoryIfMissing True $ takeDirectory path
  inputFiles <- listDirectory $ optInputImage opts
  let inputFiles' = sort $ filter isPng inputFiles
  let count = length inputFiles'
  let doOne inputFile index = do
        let inputBase = takeBaseName inputFile
        let inputPath = optInputImage opts </> inputFile
        let outputPath = (</> inputFile) <$> optOutputImage opts
        let outputCaptionPath = (</> (inputBase ++ ".txt")) <$> optOutputCaption opts
        logger $ "Processing " ++ inputPath ++ " (" ++ show index ++ "/" ++ show count ++ ")"
        result <-
          runMaybeT $
            doSingle opts $
              DoSingle
                { oneInputImage = inputPath
                , oneOutputImage = outputPath
                , oneOutputCaption = outputCaptionPath
                , oneLogger = logger
                , onePatternPairs = patternPairs
                }
        case result of
          Nothing -> pure () -- maybe add a CLI to abort on error?
          Just () -> pure ()
  mapM_ (uncurry doOne) $ zip inputFiles' [1 :: Int ..]

doNonBatch :: CLIOpts -> Logger -> [(String, String)] -> IO ()
doNonBatch opts logger patternPairs = do
  let inputFile = optInputImage opts
  let outputFile = optOutputImage opts
  let outputCaption = optOutputCaption opts
  when (optCreateMissingDirs opts) do
    case outputFile of
      Nothing -> pure ()
      Just path -> createDirectoryIfMissing True $ takeDirectory path
    case outputCaption of
      Nothing -> pure ()
      Just path -> createDirectoryIfMissing True $ takeDirectory path
  result <-
    runMaybeT $
      doSingle
        opts
        DoSingle
          { oneInputImage = inputFile
          , oneOutputImage = outputFile
          , oneOutputCaption = outputCaption
          , oneLogger = logger
          , onePatternPairs = patternPairs
          }
  case result of
    Nothing -> exitFailure
    Just () -> pure ()

main :: IO ()
main = do
  setLocaleEncoding utf8
  fmap parseCliOpts getArgs >>= validateBatch >>= \case
    Left err -> do
      putStrLn err
      putStrLn ""
      putStrLn helpMessage
      exitFailure
    Right opts -> do
      when (optHelp opts) do
        putStrLn helpMessage
        exitSuccess
      logger <- case optLogFile opts of
        Nothing -> pure putStrLn
        Just path -> do
          -- just let the runtime close the handle for us on exit
          handle <- openFile path AppendMode
          hSetBuffering handle LineBuffering
          pure \msg -> do
            hPutStrLn handle msg
            putStrLn msg
      patternPairs <- case optPatternFile opts of
        Nothing -> pure []
        Just path -> readPatternPairs path
      if optBatch opts
        then doBatch opts logger patternPairs
        else doNonBatch opts logger patternPairs
