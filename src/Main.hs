{-# LANGUAGE OverloadedStrings #-}
module Main where

import Unlit.Text
import Data.Char (toLower)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import System.Environment (getArgs,getProgName)
import System.Exit (exitWith,ExitCode(..))
import System.IO (hPutStrLn,stderr)
import System.Console.GetOpt (OptDescr(..),ArgDescr(..),ArgOrder(..),usageInfo,getOpt)

str2name :: String -> Maybe Name
str2name arg = case map toLower arg of
  "all"      -> Just All
  "bird"     -> Just Bird
  "haskell"  -> Just Haskell
  "latex"    -> Just LaTeX
  "markdown" -> Just Markdown
  "code"     -> Nothing
  _          -> error ("non-existent style " ++ arg)

data Options = Options
  { optSourceStyle :: Maybe Style
  , optTargetStyle :: Maybe Name
  , optInputFile   :: IO Text
  , optOutputFile  :: Text -> IO ()
  }

defaultOptions :: Options
defaultOptions = Options
  { optSourceStyle = Nothing
  , optTargetStyle = Nothing
  , optInputFile   = T.getContents
  , optOutputFile  = T.putStrLn
  }

options :: [ OptDescr (Options -> IO Options) ]
options =
  [ Option "s" ["source"]
    (ReqArg (\arg opt -> return opt { optSourceStyle = fmap name2style (str2name arg) })
            "STYLE_NAME")
    "Source style (all, bird, haskell, latex, markdown)"
  , Option "t" ["target"]
    (ReqArg (\arg opt -> return opt { optTargetStyle = str2name arg })
            "STYLE_NAME")
    "Target style (bird, latex, markdown, code)"
  , Option "i" ["input"]
    (ReqArg (\arg opt -> return opt { optInputFile = T.readFile arg })
            "FILE")
    "Input file (optional)"
  , Option "o" ["output"]
    (ReqArg (\arg opt -> return opt { optOutputFile = T.writeFile arg })
            "FILE")
    "Output file (optional)"
  , Option "h" ["help"]
    (NoArg  (\_ -> do
    	      prg <- getProgName
              hPutStrLn stderr (usageInfo prg options)
              exitWith ExitSuccess))
    "Show help"
  ]

main :: IO ()
main = do
  args <- getArgs

  -- parse options
  let (actions, nonOptions, errors) = getOpt Permute options args
  opts <- foldl (>>=) (return defaultOptions) actions
  let Options { optSourceStyle = ss
              , optTargetStyle = ts
              , optInputFile   = input
              , optOutputFile  = output } = opts
  -- define unlit/relit
  let run = maybe (unlit ss) (relit ss) ts

  -- run unlit/relit
  input >>= output . run
