{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Char (toLower)
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

parseStyle :: String -> Style
parseStyle arg = case map toLower arg of
  "all"           -> all
  "bird"          -> bird
  "jekyll"        -> jekyll
  "haskell"       -> haskell
  "latex"         -> latex
  "markdown"      -> markdown
  "tildefence"    -> tildefence
  "backtickfence" -> backtickfence
  "code"          -> []
  _               -> error ("non-existent style " ++ arg)

data Options = Options
  { optSourceStyle :: Style
  , optTargetStyle :: Style
  , optInputFile   :: IO Text
  , optOutputFile  :: Text -> IO ()
  , optWsMode      :: WhitespaceMode
  , optGhc         :: Bool
  , optLanguage    :: Maybe Lang
  }

defaultOptions :: Options
defaultOptions = Options
  { optSourceStyle = []
  , optTargetStyle = []
  , optInputFile   = T.getContents
  , optOutputFile  = T.putStrLn
  , optWsMode      = KeepIndent
  , optGhc         = False
  , optLanguage    = Nothing
  }

parseWsMode :: String -> WhitespaceMode
parseWsMode str = case map toLower str of
  "keep-all" -> KeepAll
  _          -> KeepIndent

options :: [ OptDescr (Options -> IO Options) ]
options =
  [ Option "f" ["from"]
    (ReqArg (\arg opt -> return opt { optSourceStyle = optSourceStyle opt ++ parseStyle arg })
            "STYLE_NAME")
    "Source style (all, bird, jekyll, haskell, latex, markdown, tildefence, backtickfence)"
  , Option "t" ["to"]
    (ReqArg (\arg opt -> return opt { optTargetStyle = parseStyle arg })
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
    (ReqArg (\arg opt -> return opt { optWsMode = parseWsMode arg })
            "WHITESPACE_MODE")
    "Whitespace mode (keep-all, keep-indent)"

  , Option [] ["language"]
    (ReqArg (\arg opt -> return opt { optLanguage = Just (T.pack arg) })
            "LANGUAGE")
    "Programming language (restrict fenced code blocks)"

  , Option [] ["help"]
    (NoArg  (\_ -> do
              prg <- getProgName
              hPutStrLn stderr (usageInfo prg options)
              args <- getArgs
              print args
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
  let Options { optSourceStyle = ss
              , optTargetStyle = ts
              , optInputFile   = istream
              , optOutputFile  = ostream
              , optWsMode      = wsmode
              , optGhc         = ghc
              , optLanguage    = lang
              } = opts

  let istream' = if ghc then T.readFile  (nonOptions !! 1) else istream
  let ostream' = if ghc then T.writeFile (nonOptions !! 2) else ostream

  let ss' = maybe ss (\l -> forLang l ss) lang
  let ts' = maybe ts (\l -> forLang l ts) lang

  -- define unlit/relit
  let run = either (error . unpack . showError) id . if null ts then unlit wsmode ss' else relit ss' ts'

  -- run unlit/relit
  istream' >>= ostream' . run
