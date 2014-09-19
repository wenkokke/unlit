{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad (when)
import           Data.Char (toLower)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import           Data.Version (showVersion)
import           Paths_unlit (version)
import           Prelude hiding (all)
import           System.Console.GetOpt (OptDescr(..),ArgDescr(..),ArgOrder(..),usageInfo,getOpt)
import           System.Directory (doesFileExist)
import           System.Environment (getArgs,getProgName)
import           System.Exit (exitSuccess)
import           System.IO (hPutStrLn,stderr)
import           Unlit.Text

parseStyle :: String -> Style
parseStyle arg = case map toLower arg of
  "all"           -> all
  "bird"          -> bird
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
  , optLanguage    :: Maybe Language
  }

defaultOptions :: Options
defaultOptions = Options
  { optSourceStyle = []
  , optTargetStyle = []
  , optInputFile   = T.getContents
  , optOutputFile  = T.putStrLn
  , optLanguage    = Nothing
  }

options :: [ OptDescr (Options -> IO Options) ]
options =
  [ Option "f" ["from"]
    (ReqArg (\arg opt -> return opt { optSourceStyle = (optSourceStyle opt) ++ (parseStyle arg) })
            "STYLE_NAME")
    "Source style (all, bird, haskell, latex, markdown, tildefence, backtickfence)"
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
  , Option "l" ["language"]
    (ReqArg (\arg opt -> return opt { optLanguage = Just (T.pack arg) })
            "LANGUAGE")
    "Programming language (restrict fenced code blocks)"

  , Option "h" ["help"]
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

  -- compatibility with GHC calling conventions
  when (length args == 2) $ do
    let ifile = args !! 0
    let ofile = args !! 1

    iokay <- fmap (ifile == "-" ||) (doesFileExist ifile)
    when iokay $ do

      ookay <- fmap (ofile == "-" ||) (doesFileExist ofile)
      when ookay $ do

        let istream = if ifile == "-" then T.getContents else T.readFile ifile
        let ostream = if ofile == "-" then T.putStrLn else T.writeFile ofile
        istream >>= ostream . unlit haskell
        exitSuccess


  -- otherwise use my own calling conventions
  let (actions, nonOptions, errors) = getOpt Permute options args
  opts <- foldl (>>=) (return defaultOptions) actions
  let Options { optSourceStyle = ss
              , optTargetStyle = ts
              , optInputFile   = istream
              , optOutputFile  = ostream
              , optLanguage    = lang } = opts

  let ss' = maybe ss (\l -> forLanguage l ss) lang
  let ts' = maybe ts (\l -> forLanguage l ts) lang

  -- define unlit/relit
  let run = if null ts then unlit ss' else relit ss' ts'

  -- run unlit/relit
  istream >>= ostream . run
