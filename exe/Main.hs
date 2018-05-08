{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Maybe (fromMaybe)
import           Data.Text (Text, unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Version (showVersion)
import           Paths_unlit (version)
import           Prelude hiding (all)
import           System.Console.GetOpt (OptDescr(..),ArgDescr(..),ArgOrder(..),usageInfo,getOpt)
import           System.Environment (getArgs,getProgName)
import           System.Exit (exitSuccess)
import           System.IO (hPutStrLn,stderr)
import           Unlit.Text

data Options = Options
  { optSourceStyle :: Style
  , optTargetStyle :: Style
  , optInputFile   :: IO Text
  , optOutputFile  :: Text -> IO ()
  , optWsMode      :: WhitespaceMode
  , optGhc         :: Bool
  , optLanguage    :: Lang
  }

defaultOptions :: Options
defaultOptions = Options
  { optSourceStyle = []
  , optTargetStyle = []
  , optInputFile   = T.getContents
  , optOutputFile  = T.putStrLn
  , optWsMode      = WsKeepIndent
  , optGhc         = False
  , optLanguage    = Nothing
  }

parseStyle' :: String -> Style
parseStyle' arg = fromMaybe (error $ "non-existent style" ++ arg) $ parseStyle $ T.pack arg

parseWhitespaceMode' :: String -> WhitespaceMode
parseWhitespaceMode' arg = fromMaybe (error $ "non-existent whitespace mode" ++ arg) $ parseWhitespaceMode $ T.pack arg

options :: [ OptDescr (Options -> IO Options) ]
options =
  [ Option "f" ["from"]
    (ReqArg (\arg opt -> return opt { optSourceStyle = optSourceStyle opt ++ parseStyle' arg })
            "STYLE_NAME")
    "Source style (all, bird, jekyll, haskell, latex, markdown, tildefence, backtickfence)"
  , Option "t" ["to"]
    (ReqArg (\arg opt -> return opt { optTargetStyle = parseStyle' arg })
            "STYLE_NAME")
    "Target style (bird, latex, tildefence, backtickfence, code)"

  , Option "i" ["input"]
    (ReqArg (\arg opt -> return opt { optInputFile = T.readFile arg })
            "FILE")
    "Input file (optional)"
  , Option "o" ["output"]
    (ReqArg (\arg opt -> return opt { optOutputFile = T.writeFile arg })
            "FILE")
    "Output file (optional)"

  , Option [] ["ghc"]
    (NoArg (\opt -> return opt { optGhc = True }))
    "Follow GHC calling conventions"

  , Option [] ["ws-mode"]
    (ReqArg (\arg opt -> return opt { optWsMode = parseWhitespaceMode' arg })
            "WHITESPACE_MODE")
    "Whitespace mode (all, indent)"

  , Option [] ["language"]
    (ReqArg (\arg opt -> return opt { optLanguage = Just (T.pack arg) })
            "LANGUAGE")
    "Programming language (restrict fenced code blocks)"

  , Option [] ["help"]
    (NoArg  (\_ -> do
              prg <- getProgName
              hPutStrLn stderr (usageInfo prg options)
              exitSuccess))
    "Show help"

  , Option "v" ["version"]
    (NoArg (\_ -> do
              hPutStrLn stderr ("unlit version " ++ showVersion version)
              exitSuccess))
    "Show version"
  ]

main :: IO ()
main = do
  args <- getArgs

  -- use my own calling conventions
  let (actions, nonOptions, _errors) = getOpt Permute options args
  opts <- foldl (>>=) (return defaultOptions) actions

  let ss = setLang (optLanguage opts) (optSourceStyle opts)
      ts = setLang (optLanguage opts) (optTargetStyle opts)
      (istream, ostream) =
        case nonOptions of
          (i:o:_) | optGhc opts -> (T.readFile i, T.writeFile o)
                  | otherwise -> error "Two arguments required: Input and output file"
          _ -> (optInputFile opts, optOutputFile opts)

  istream >>= either (error . unpack . showError) ostream .
    if null ts then unlit (optWsMode opts) ss else relit ss (head ts)
