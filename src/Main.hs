{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant fmap" #-}
{-# HLINT ignore "Redundant pure" #-}
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
import Control.Exception (assert, evaluate)
import Control.Monad (foldM, when, (<=<))
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List (foldl', intercalate, stripPrefix)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

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
  | KeySize
  | KeyModelHash
  | KeyModel
  | KeyLoraHashes
  | KeyVersion
  | KeyTemplate
  | KeyNegativeTemplate
  deriving (Bounded, Enum, Eq, Ord, Show)

keyToText :: Key -> String
keyToText key = case key of
  KeyPositivePrompt -> "Positive prompt"
  KeyNegativePrompt -> "Negative prompt"
  KeySteps -> "Steps"
  KeySampler -> "Sampler"
  KeyCFGScale -> "CFG scale"
  KeySeed -> "Seed"
  KeySize -> "Size"
  KeyModelHash -> "Model hash"
  KeyModel -> "Model"
  KeyLoraHashes -> "Lora hashes"
  KeyVersion -> "Version"
  KeyTemplate -> "Template"
  KeyNegativeTemplate -> "Negative Template"

data SDMetadata = SDMetadata
  { sdPositivePrompt :: (String, [SDExtension])
  , sdNegativePrompt :: (String, [SDExtension])
  , sdSteps :: Int
  , sdSampler :: String
  , sdCfgScale :: Int
  , sdSeed :: Int
  , sdSize :: (Int, Int)
  , sdModelHash :: String
  , sdModel :: String
  , sdLoraHashes :: [(String, String)]
  , sdVersion :: String
  , sdPositiveTemplate :: (String, [SDExtension])
  , sdNegativeTemplate :: (String, [SDExtension])
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
    , sdSize = (0, 0)
    , sdModelHash = ""
    , sdModel = ""
    , sdLoraHashes = []
    , sdVersion = ""
    , sdPositiveTemplate = ("", [])
    , sdNegativeTemplate = ("", [])
    }

data SDMetadataEntry = SDMetadataEntry
  { sdKey :: String -- positivePrompt uses empty string as key
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
     in Just $ go [name] rest'
  _ -> Nothing
 where
  spanItem = span (`notElem` (":>" :: String))

  go :: [String] -> String -> (String, SDExtension)
  go acc = \case
    ':' : argBegin ->
      let (arg, rest) = spanItem argBegin
       in go (arg : acc) rest
    '>' : rest ->
      let name : args = reverse $ map trim acc
       in (rest, SDExtension name args)
    rest -> error $ "stripSdExtension: invalid SD extension syntax -- " ++ show rest

-- Removes the extension encoding from the input string and returns the extensions.
extractSdExtensions :: String -> (String, [SDExtension])
extractSdExtensions = go ("", [])
 where
  go (accText, accExts) = \case
    (stripSdExtension -> Just (rest, ext)) -> go (accText, ext : accExts) rest
    (c : rest) -> go (c : accText, accExts) rest
    [] -> (trimHeavy $ reverse accText, reverse accExts)

trim :: String -> String
trim = f . f
 where
  f = reverse . dropWhile (== ' ')

-- trims front and tail and squishes multiple spaces into one
-- replaces all non-space whitespace with spaces
trimHeavy :: String -> String
trimHeavy = unwords . words

parseSdMetadata :: String -> SDMetadata
parseSdMetadata text = foldl' accumSdMetadata emptySdMetadata theLines
 where
  theLines = map trim case lines text of
    pos : neg : commaDelimJunk : rest -> pos : neg : splitOn "," commaDelimJunk ++ rest
    _ -> error "parseSdMetadata: not enough lines"

accumSdMetadata :: SDMetadata -> String -> SDMetadata
accumSdMetadata sd theLine = case theLine of
  (goEntry KeyNegativePrompt -> Just entry) -> sd{sdNegativePrompt = extractSdExtensions $ sdValue entry}
  (goEntry KeySteps -> Just entry) -> sd{sdSteps = read $ sdValue entry}
  (goEntry KeySampler -> Just entry) -> sd{sdSampler = sdValue entry}
  (goEntry KeyCFGScale -> Just entry) -> sd{sdCfgScale = read $ sdValue entry}
  (goEntry KeySeed -> Just entry) -> sd{sdSeed = read $ sdValue entry}
  (goEntry KeySize -> Just entry) -> sd{sdSize = readSize $ sdValue entry}
  (goEntry KeyModelHash -> Just entry) -> sd{sdModelHash = sdValue entry}
  (goEntry KeyModel -> Just entry) -> sd{sdModel = sdValue entry}
  (goEntry KeyLoraHashes -> Just entry) -> sd{sdLoraHashes = readLoraHashes $ sdValue entry}
  (goEntry KeyVersion -> Just entry) -> sd{sdVersion = sdValue entry}
  (goEntry KeyTemplate -> Just entry) -> sd{sdPositiveTemplate = extractSdExtensions $ sdValue entry}
  (goEntry KeyNegativeTemplate -> Just entry) -> sd{sdNegativeTemplate = extractSdExtensions $ sdValue entry}
  _ -> case sdPositivePrompt sd of
    ("", []) -> sd{sdPositivePrompt = extractSdExtensions theLine}
    _ -> error $ "parse error: " ++ theLine ++ " is not a valid SD metadata entry"
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
toParameters sd = (key, value)
 where
  key = J.Unknown "parameters"
  showKey = (++ ": ") . keyToText
  value =
    J.String $
      unlines $
        catMaybes
          [ Just $ inlineSdExtensions $ sdPositivePrompt sd
          , Just $ showKey KeyNegativePrompt ++ inlineSdExtensions (sdNegativePrompt sd)
          , Just $ intercalate ", " commaDelimJunk
          , if null $ sdPositiveTemplate sd
              then Nothing
              else Just $ showKey KeyTemplate ++ inlineSdExtensions (sdPositiveTemplate sd)
          , if null $ sdNegativeTemplate sd
              then Nothing
              else Just $ showKey KeyNegativeTemplate ++ inlineSdExtensions (sdNegativeTemplate sd)
          ]
  commaDelimJunk =
    [ showKey KeySteps ++ show (sdSteps sd)
    , showKey KeySampler ++ sdSampler sd
    , showKey KeyCFGScale ++ show (sdCfgScale sd)
    , showKey KeySeed ++ show (sdSeed sd)
    , showKey KeySize ++ show (fst $ sdSize sd) ++ "x" ++ show (snd $ sdSize sd)
    , showKey KeyModelHash ++ sdModelHash sd
    , showKey KeyModel ++ sdModel sd
    , showKey KeyLoraHashes ++ intercalate "," (map (\(k, v) -> k ++ ": " ++ v) $ sdLoraHashes sd)
    , showKey KeyVersion ++ sdVersion sd
    ]

prettySdMetadata :: SDMetadata -> String
prettySdMetadata sd =
  intercalate "\n" $
    catMaybes
      [ showKey KeyPositivePrompt $ inlineSdExtensions (sdPositivePrompt sd)
      , showKey KeyNegativePrompt $ inlineSdExtensions (sdNegativePrompt sd)
      , showKey KeySteps $ show (sdSteps sd)
      , showKey KeySampler $ sdSampler sd
      , showKey KeyCFGScale $ show (sdCfgScale sd)
      , showKey KeySeed $ show (sdSeed sd)
      , showKey KeySize $ show (fst $ sdSize sd) ++ "x" ++ show (snd $ sdSize sd)
      , showKey KeyModelHash $ sdModelHash sd
      , showKey KeyModel $ sdModel sd
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
  showKey key str = Just $ keyToText key ++ ": " ++ str

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

editSdMetadata :: (String, String) -> SDMetadata -> SDMetadata
editSdMetadata (key, val) = case key of
  (eq KeyPositivePrompt -> True) -> \sd -> sd{sdPositivePrompt = extractSdExtensions val}
  (eq KeyNegativePrompt -> True) -> \sd -> sd{sdNegativePrompt = extractSdExtensions val}
  (eq KeySteps -> True) -> \sd -> sd{sdSteps = read val}
  (eq KeySampler -> True) -> \sd -> sd{sdSampler = val}
  (eq KeyCFGScale -> True) -> \sd -> sd{sdCfgScale = read val}
  (eq KeySeed -> True) -> \sd -> sd{sdSeed = read val}
  (eq KeySize -> True) -> \sd -> sd{sdSize = readSize val}
  (eq KeyModelHash -> True) -> \sd -> sd{sdModelHash = val}
  (eq KeyModel -> True) -> \sd -> sd{sdModel = val}
  (eq KeyLoraHashes -> True) -> \sd -> sd{sdLoraHashes = readLoraHashes val}
  (eq KeyVersion -> True) -> \sd -> sd{sdVersion = val}
  (eq KeyTemplate -> True) -> \sd -> sd{sdPositiveTemplate = extractSdExtensions val}
  (eq KeyNegativeTemplate -> True) -> \sd -> sd{sdNegativeTemplate = extractSdExtensions val}
  _ -> error $ "Unknown key: " ++ key
 where
  eq k s = keyToText k == s

data CLIOpts = CLIOpts
  { optHelp :: Bool
  , optInputImage :: FilePath
  , optOutputImage :: Maybe FilePath
  , optOutputCaption :: Maybe FilePath
  , optPrint :: Bool
  , optOverwriteInput :: Bool
  , optForce :: Bool
  , optEdit :: [(String, String)]
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
    , optOverwriteInput = False
    , optForce = False
    , optEdit = []
    }

-- Usage:
--  sd-png-metadata <options>
--
-- Options
--  --help               Show this help text.
--  --input-image        Input image file path. Required.
--  --output-image       Output image file path.
--  --output-caption     Output caption file path.
--  --print              Print the metadata to stdout.
--  --overwrite-input    Overwrite the input image with the metadata.
--  --force              Disable all overwrite checks.
--  --edit <key> <value> Edit the metadata.
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
    , " --help               Show this help text."
    , " --input-image        Input image file path. Required."
    , " --output-image       Output image file path."
    , " --output-caption     Output caption file path."
    , " --print              Print the input image metadata to stdout."
    , " --overwrite-input    Overwrite the input image with the metadata."
    , " --force              Disable all overwrite checks."
    , " --edit <key> <value> Edit the metadata."
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
    "--print" : rest -> go opts{optPrint = True} rest
    "--overwrite-input" : rest -> go opts{optOverwriteInput = True} rest
    "--force" : rest -> go opts{optForce = True} rest
    "--edit" : key : value : rest -> go opts{optEdit = (key, value) : optEdit opts} rest
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
    let handleSpecialPrint o = case (optOutputImage o, optOutputCaption o) of
          (Nothing, Nothing) -> Right o{optPrint = True}
          _ -> Right o
    let fixups =
          [ validateInput
          , handleOverwrite
          , handleSpecialPrint
          ]
    if optHelp opts
      then pure opts
      else foldM (\o f -> f o) opts fixups

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

main :: IO ()
main = do
  fmap parseCliOpts getArgs >>= \case
    Left err -> do
      putStrLn err
      putStrLn ""
      putStrLn helpMessage
      exitFailure
    Right opts -> do
      when (optHelp opts) do
        putStrLn helpMessage
        exitSuccess
      let inputFile = optInputImage opts
      let existsCheck path = case optForce opts of
            True -> pure ()
            False -> do
              exists <- doesFileExist path
              when exists do
                putStrLn $ "Output file already exists: " ++ path
                exitFailure
      readImageWithMetadata inputFile >>= \case
        Left err -> putStrLn $ "Error loading image: " ++ err
        Right (dynImage, metadatas) -> do
          let image = convertRGBA8 dynImage
          let parameters = J.lookup (J.Unknown "parameters") metadatas
          case parameters of
            Nothing -> putStrLn "No parameters metadata found"
            Just (J.String text) -> do
              let sd = parseSdMetadata text
              when (optPrint opts) do
                putStrLn $ prettySdMetadata sd
              let sd' = foldr editSdMetadata sd (optEdit opts)
              let metadatas' = applyToMetadatas sd' metadatas
              forceEvalMetadatas metadatas' -- forces out error messages before we start writing files
              case optOutputImage opts of
                Nothing -> pure ()
                Just path -> do
                  if optOverwriteInput opts
                    then do
                      assert (path == inputFile) $ pure ()
                      -- XXX: a less silly way of doing this would be to load as a ByteString, evaluate that instead, and then decode the image and metadata
                      forceEvalImage image
                    else do
                      existsCheck path
                  let bs = encodePngWithMetadata metadatas' image
                  BS.writeFile path bs
              case optOutputCaption opts of
                Nothing -> pure ()
                Just path -> do
                  existsCheck path
                  let caption = inlineSdExtensions $ sdPositivePrompt sd'
                  writeFile path caption
            Just val -> putStrLn $ "Unexpected \"parameters\" metadata value: " ++ show val
