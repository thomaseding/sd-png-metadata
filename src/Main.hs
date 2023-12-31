{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant fmap" #-}
{-# HLINT ignore "Redundant pure" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
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
import Data.List (
  foldl',
  intercalate,
  partition,
  sort,
  stripPrefix,
 )
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import GHC.Stack (HasCallStack)
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

myRead :: HasCallStack => Read a => String -> a
myRead s = case reads s of
  [(x, "")] -> x
  _ -> error $ "myRead: cannot read " ++ show s

trim :: String -> String
trim = f . f
 where
  f = reverse . dropWhile (== ' ')

-- trims front and tail and squishes multiple spaces into one
-- replaces all non-space whitespace with spaces
trimHeavy :: String -> String
trimHeavy = unwords . words

isDecimalNumber :: String -> Bool
isDecimalNumber ('-' : rest) = isDecimalNumber rest
isDecimalNumber s = case partition (== '.') s of
  ('.' : '.' : _, _) -> False
  (_, s') -> all (`elem` ['0' .. '9']) s'

isDotDec :: Char -> Bool
isDotDec c = c `elem` ('.' : ['0' .. '9'])

newtype FloatString = FloatString {unFloatString :: String}
  deriving (Eq, Ord, Show)

instance Read FloatString where
  readsPrec _ s = case partition isDotDec s of
    (s', _) -> case isDecimalNumber s' of
      True -> [(FloatString s', "")]
      False -> []

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
  | KeyControlNet (Maybe Int)
  deriving (Eq, Ord, Show)

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
  KeyControlNet Nothing -> "ControlNet"
  KeyControlNet (Just n) -> "ControlNet " ++ show n

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
  (eqKeyControlNet -> Just n) -> Just $ KeyControlNet n
  _ -> Nothing
 where
  eq k s = keyToText k == s
  eqKeyControlNet s = case stripPrefix (keyToText $ KeyControlNet Nothing) s of
    Nothing -> Nothing
    Just s' -> case stripPrefix " " s' of
      Nothing -> case s' of
        "" -> Just Nothing
        _ -> Nothing
      Just s'' -> case all (`elem` ['0' .. '9']) s'' of
        True -> Just $ Just $ myRead s''
        False -> Nothing

data SDMetadata = SDMetadata
  { sdPositivePrompt :: (String, [SDExtension])
  , sdNegativePrompt :: (String, [SDExtension])
  , sdSteps :: Int
  , sdSampler :: String
  , sdCfgScale :: FloatString
  , sdSeed :: Int
  , sdFaceRestoration :: String
  , sdSize :: (Int, Int)
  , sdModelHash :: String
  , sdModel :: String
  , sdClipSkip :: Maybe Int
  , sdCfgRescalePhi :: Maybe FloatString
  , sdLoraHashes :: [(String, String)]
  , sdVersion :: String
  , sdPositiveTemplate :: (String, [SDExtension])
  , sdNegativeTemplate :: (String, [SDExtension])
  , sdControlNets :: Map.Map (Maybe Int) String
  }
  deriving (Show)

emptySdMetadata :: SDMetadata
emptySdMetadata =
  SDMetadata
    { sdPositivePrompt = ("", [])
    , sdNegativePrompt = ("", [])
    , sdSteps = 0
    , sdSampler = ""
    , sdCfgScale = read "0"
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
    , sdControlNets = Map.empty
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

stripSdExtension :: HasCallStack => String -> Maybe (String, SDExtension)
stripSdExtension = \case
  '<' : rest ->
    let (name, rest') = spanItem rest
     in go [name] rest'
  _ -> Nothing
 where
  spanItem = span (`notElem` (":>" :: String))

  go :: HasCallStack => [String] -> String -> Maybe (String, SDExtension)
  go acc = \case
    ':' : argBegin ->
      let (arg, rest) = spanItem argBegin
       in go (arg : acc) rest
    '>' : rest -> case reverse $ map trim acc of
      [] -> error "impossible?"
      name : args -> Just (rest, SDExtension name args)
    _ -> Nothing

-- Removes the extension encoding from the input string and returns the extensions.
extractSdExtensions :: HasCallStack => String -> (String, [SDExtension])
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
stripSdWeightedText :: HasCallStack => String -> Maybe (String, SDWeightedText)
stripSdWeightedText = \case
  '(' : rest ->
    let (text, rest') = span (`notElem` ":)") rest
     in go text "1.1" rest'
  _ -> Nothing
 where
  go :: HasCallStack => String -> String -> String -> Maybe (String, SDWeightedText)
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
removeTextWeights' :: HasCallStack => String -> String
removeTextWeights' = go ""
 where
  go acc = \case
    (stripSdWeightedText -> Just (rest, weighted)) -> go (reverse (sdText weighted) ++ acc) rest
    (c : rest) -> go (c : acc) rest
    [] -> reverse acc

-- This is done to work around `stripSdWeightedText` not handling nested weighted text
removeTextWeights :: HasCallStack => String -> String
removeTextWeights s = case s == s' of
  True -> s
  False -> removeTextWeights' s'
 where
  s' = removeTextWeights' s

massageLines :: HasCallStack => String -> String
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

splitUnquotedCommas :: HasCallStack => String -> [String]
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

parseSdMetadata :: HasCallStack => String -> SDMetadata
parseSdMetadata text = foldl' accumSdMetadata emptySdMetadata theLines
 where
  theLines = map trim case lines text of
    pos : (viewNegativePrompt -> Just neg) : commaDelimJunk : rest -> pos : neg : splitUnquotedCommas commaDelimJunk ++ rest
    pos : commaDelimJunk : rest -> pos : splitUnquotedCommas commaDelimJunk ++ rest
    _ -> error "parseSdMetadata: not enough lines"

accumSdMetadata :: HasCallStack => SDMetadata -> String -> SDMetadata
accumSdMetadata sd theLine = case theLine of
  (goEntry KeyNegativePrompt -> Just entry) -> sd{sdNegativePrompt = extractSdExtensions $ sdValue entry}
  (goEntry KeySteps -> Just entry) -> sd{sdSteps = myRead $ sdValue entry}
  (goEntry KeySampler -> Just entry) -> sd{sdSampler = sdValue entry}
  (goEntry KeyCFGScale -> Just entry) -> sd{sdCfgScale = myRead $ sdValue entry}
  (goEntry KeySeed -> Just entry) -> sd{sdSeed = myRead $ sdValue entry}
  (goEntry KeyFaceRestoration -> Just entry) -> sd{sdFaceRestoration = sdValue entry}
  (goEntry KeySize -> Just entry) -> sd{sdSize = readSize $ sdValue entry}
  (goEntry KeyModelHash -> Just entry) -> sd{sdModelHash = sdValue entry}
  (goEntry KeyModel -> Just entry) -> sd{sdModel = sdValue entry}
  (goEntry KeyClipSkip -> Just entry) -> sd{sdClipSkip = Just $ myRead $ sdValue entry}
  (goEntry KeyCFGRescalePhi -> Just entry) -> sd{sdCfgRescalePhi = Just $ myRead $ sdValue entry}
  (goEntry KeyLoraHashes -> Just entry) -> sd{sdLoraHashes = readLoraHashes $ sdValue entry}
  (goEntry KeyVersion -> Just entry) -> sd{sdVersion = sdValue entry}
  (goEntry KeyTemplate -> Just entry) -> sd{sdPositiveTemplate = extractSdExtensions $ sdValue entry}
  (goEntry KeyNegativeTemplate -> Just entry) -> sd{sdNegativeTemplate = extractSdExtensions $ sdValue entry}
  (goEntry (KeyControlNet Nothing) -> Just entry) -> sd{sdControlNets = Map.insert Nothing (sdValue entry) $ sdControlNets sd}
  (goEntry (KeyControlNet (Just 0)) -> Just entry) -> sd{sdControlNets = Map.insert (Just 0) (sdValue entry) $ sdControlNets sd}
  (goEntry (KeyControlNet (Just 1)) -> Just entry) -> sd{sdControlNets = Map.insert (Just 1) (sdValue entry) $ sdControlNets sd}
  (goEntry (KeyControlNet (Just 2)) -> Just entry) -> sd{sdControlNets = Map.insert (Just 2) (sdValue entry) $ sdControlNets sd}
  (goEntry (KeyControlNet (Just 3)) -> Just entry) -> sd{sdControlNets = Map.insert (Just 3) (sdValue entry) $ sdControlNets sd}
  (goEntry (KeyControlNet (Just 4)) -> Just entry) -> sd{sdControlNets = Map.insert (Just 4) (sdValue entry) $ sdControlNets sd}
  (goEntry (KeyControlNet (Just 5)) -> Just entry) -> sd{sdControlNets = Map.insert (Just 5) (sdValue entry) $ sdControlNets sd}
  (goEntry (KeyControlNet (Just 6)) -> Just entry) -> sd{sdControlNets = Map.insert (Just 6) (sdValue entry) $ sdControlNets sd}
  (goEntry (KeyControlNet (Just 7)) -> Just entry) -> sd{sdControlNets = Map.insert (Just 7) (sdValue entry) $ sdControlNets sd}
  (goEntry (KeyControlNet (Just 8)) -> Just entry) -> sd{sdControlNets = Map.insert (Just 8) (sdValue entry) $ sdControlNets sd}
  (goEntry (KeyControlNet (Just 9)) -> Just entry) -> sd{sdControlNets = Map.insert (Just 9) (sdValue entry) $ sdControlNets sd}
  -- KeyPositivePrompt is not keyed by string, so we have to do this special case
  _ -> case sdPositivePrompt sd of
    ("", []) -> sd{sdPositivePrompt = extractSdExtensions theLine}
    _ -> error $ "parse error: " ++ show theLine ++ " is not a valid SD metadata entry"
 where
  goEntry = parseSdMetadataEntry . keyToText

parseSdMetadataEntry :: HasCallStack => String -> String -> Maybe SDMetadataEntry
parseSdMetadataEntry key text = case stripPrefix key' text of
  Nothing -> Nothing
  Just text' -> Just $ SDMetadataEntry key $ trim text'
 where
  key' = key ++ ":"

isNegativePrompt :: HasCallStack => String -> Bool
isNegativePrompt s = case parseSdMetadataEntry (keyToText KeyNegativePrompt) s of
  Just _ -> True
  Nothing -> False

viewNegativePrompt :: HasCallStack => String -> Maybe String
viewNegativePrompt s = case isNegativePrompt s of
  True -> Just s
  False -> Nothing

readSize :: HasCallStack => String -> (Int, Int)
readSize text = (myRead w, myRead h)
 where
  (w, h) = case break (== 'x') text of
    (w', _ : h') -> (w', h')
    _ -> error $ "readSize: invalid size: " ++ show text

readLoraHashes :: HasCallStack => String -> [(String, String)]
readLoraHashes text = map readLoraHash $ splitOn "," text

readLoraHash :: HasCallStack => String -> (String, String)
readLoraHash text = (key, trim value)
 where
  (key, value) = case break (== ':') text of
    (key', _ : value') -> (key', value')
    _ -> error $ "readLoraHash: invalid lora hash: " ++ show text

inlineSdExtensions :: HasCallStack => (String, [SDExtension]) -> String
inlineSdExtensions (text, exts) = text ++ concatMap inlineSdExtension exts
 where
  inlineSdExtension (SDExtension name args) = " <" ++ name ++ concatMap (':' :) args ++ ">"

toParameters :: HasCallStack => SDMetadata -> (J.Keys J.Value, J.Value)
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

prettySdMetadata :: HasCallStack => Bool -> [Key] -> SDMetadata -> String
prettySdMetadata includeKey whitelist sd =
  intercalate "\n" $
    concat
      [ showKey KeyPositivePrompt $ inlineSdExtensions (sdPositivePrompt sd)
      , showKey KeyNegativePrompt $ inlineSdExtensions (sdNegativePrompt sd)
      , showKey KeySteps $ show (sdSteps sd)
      , showKey KeySampler $ sdSampler sd
      , showKey KeyCFGScale $ show (sdCfgScale sd)
      , showKey KeySeed $ show (sdSeed sd)
      , case sdFaceRestoration sd of
          "" -> []
          face -> showKey KeyFaceRestoration face
      , showKey KeySize $ show (fst $ sdSize sd) ++ "x" ++ show (snd $ sdSize sd)
      , showKey KeyModelHash $ sdModelHash sd
      , showKey KeyModel $ sdModel sd
      , case Map.toList $ sdControlNets sd of
          [] -> []
          cns -> concatMap (\(n, cn) -> showKey (KeyControlNet n) cn) cns
      , case sdClipSkip sd of
          Nothing -> []
          Just clipSkip -> showKey KeyClipSkip $ show clipSkip
      , case sdCfgRescalePhi sd of
          Nothing -> []
          Just phi -> showKey KeyCFGRescalePhi $ show phi
      , showKey KeyLoraHashes $ intercalate "," (map (\(k, v) -> k ++ ": " ++ v) $ sdLoraHashes sd)
      , showKey KeyVersion $ sdVersion sd
      , if null $ sdPositiveTemplate sd
          then []
          else showKey KeyTemplate $ inlineSdExtensions (sdPositiveTemplate sd)
      , if null $ sdNegativeTemplate sd
          then []
          else showKey KeyNegativeTemplate $ inlineSdExtensions (sdNegativeTemplate sd)
      ]
 where
  showKey key str = case allowed key of
    True -> pure case includeKey of
      True -> keyToText key ++ ": " ++ str
      False -> str
    False -> []
  allowed key = case whitelist of
    [] -> True
    _ -> key `elem` whitelist

applyToMetadatas :: HasCallStack => SDMetadata -> J.Metadatas -> J.Metadatas
applyToMetadatas sd md =
  foldr
    ($)
    md
    [ uncurry J.insert (toParameters sd)
    , J.insert J.Width (fromIntegral $ fst $ sdSize sd)
    , J.insert J.Height (fromIntegral $ snd $ sdSize sd)
    , J.insert J.Format J.SourcePng
    ]

editSdMetadata :: HasCallStack => (Key, String) -> SDMetadata -> SDMetadata
editSdMetadata (key, val) sd = case key of
  KeyPositivePrompt -> sd{sdPositivePrompt = extractSdExtensions val}
  KeyNegativePrompt -> sd{sdNegativePrompt = extractSdExtensions val}
  KeySteps -> sd{sdSteps = myRead val}
  KeySampler -> sd{sdSampler = val}
  KeyCFGScale -> sd{sdCfgScale = myRead val}
  KeySeed -> sd{sdSeed = myRead val}
  KeyFaceRestoration -> sd{sdFaceRestoration = val}
  KeySize -> sd{sdSize = readSize val}
  KeyModelHash -> sd{sdModelHash = val}
  KeyModel -> sd{sdModel = val}
  KeyClipSkip -> sd{sdClipSkip = Just $ myRead val}
  KeyCFGRescalePhi -> sd{sdCfgRescalePhi = Just $ myRead val}
  KeyLoraHashes -> sd{sdLoraHashes = readLoraHashes val}
  KeyVersion -> sd{sdVersion = val}
  KeyTemplate -> sd{sdPositiveTemplate = extractSdExtensions val}
  KeyNegativeTemplate -> sd{sdNegativeTemplate = extractSdExtensions val}
  KeyControlNet n -> sd{sdControlNets = Map.insert n val $ sdControlNets sd}

removeMetadataPromptWeights :: HasCallStack => SDMetadata -> SDMetadata
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
    , "Supported keys:" ++ intercalate "\n  " ("" : map showKey finiteKeys ++ controlNetKeys)
    ]
 where
  showKey key = "\"" ++ keyToText key ++ "\""
  finiteKeys =
    [ KeyPositivePrompt
    , KeyNegativePrompt
    , KeySteps
    , KeySampler
    , KeyCFGScale
    , KeySeed
    , KeyFaceRestoration
    , KeySize
    , KeyModelHash
    , KeyModel
    , KeyClipSkip
    , KeyCFGRescalePhi
    , KeyLoraHashes
    , KeyVersion
    , KeyTemplate
    , KeyNegativeTemplate
    ]
  controlNetKeys =
    let cn = showKey (KeyControlNet Nothing)
     in [cn, cn ++ " <n>"]

parseCliOpts :: HasCallStack => [String] -> Either String CLIOpts
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

validateBatch :: HasCallStack => Either String CLIOpts -> IO (Either String CLIOpts)
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

forceEvalImage :: HasCallStack => Image PixelRGBA8 -> IO ()
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

forceEvalMetadatas :: HasCallStack => J.Metadatas -> IO ()
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

readPatternPairs :: HasCallStack => FilePath -> IO [(String, String)]
readPatternPairs path = do
  contents <- readFile path
  let patterns = filter (not . null) $ map trim $ lines contents
  pure $ makePatternPairs patterns

stripSuffix :: HasCallStack => String -> String -> Maybe String
stripSuffix suffix = fmap reverse . stripPrefix (reverse suffix) . reverse

stripPrefixAndSuffix :: HasCallStack => String -> String -> String -> Maybe String
stripPrefixAndSuffix prefix suffix str = do
  str' <- stripPrefix prefix str
  stripSuffix suffix str'

extractSubject :: HasCallStack => [(String, String)] -> String -> Maybe String
extractSubject [] str = Just str
extractSubject patternPairs str = msum do
  (prefix, suffix) <- patternPairs
  pure $ stripPrefixAndSuffix prefix suffix str

data DoSingle = DoSingle
  { oneInputImage :: FilePath
  , oneOutputImage :: Maybe FilePath
  , oneOutputCaption :: Maybe FilePath
  , oneLogger :: String -> IO ()
  , onePatternPairs :: [(String, String)]
  }

doSingle :: HasCallStack => CLIOpts -> DoSingle -> MaybeT IO ()
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

doBatch :: HasCallStack => CLIOpts -> Logger -> [(String, String)] -> IO ()
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

doNonBatch :: HasCallStack => CLIOpts -> Logger -> [(String, String)] -> IO ()
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

main :: HasCallStack => IO ()
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
