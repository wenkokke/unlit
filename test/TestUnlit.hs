import Control.Monad (forM_, when, unless)
import Data.Algorithm.Diff (getGroupedDiff)
import Data.Algorithm.DiffOutput (ppDiff)
import Data.List (delete,isSuffixOf)
import System.IO (hPutStrLn,stderr)
import System.Directory (getDirectoryContents)
import System.Exit (ExitCode(..), exitSuccess, exitFailure)
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)

testdir, ghcUnlit, myUnlit :: String
testdir  = "/Users/pepijn/Projects/unlit/test/ghc"
ghcUnlit = "/Users/pepijn/Projects/unlit/test/ghcunlit"
myUnlit  = "/Users/pepijn/Library/Haskell/bin/unlit"

runGhcUnlit :: String -> IO String
runGhcUnlit ifile = do
  (exitCode, stdout, stderr) <- readProcessWithExitCode ghcUnlit [ifile, "-"] ""
  case exitCode of
   ExitSuccess   -> return stdout
   ExitFailure n -> return stderr

runMyUnlit :: String -> IO String
runMyUnlit ifile = do
  (exitCode, stdout, stderr) <- readProcessWithExitCode myUnlit ["--ws-mode=keep-all","-f","haskell","-i",ifile] ""
  case exitCode of
   ExitSuccess   -> return stdout
   ExitFailure n -> return stderr

removeBlankLines :: [String] -> [String]
removeBlankLines = filter (not . null)

breakAndMark :: String -> [String]
breakAndMark = zipWith (\n -> (show n ++)) [1..] . lines

main :: IO ()
main = do
  files <- fmap (delete "." . delete "..") (getDirectoryContents testdir)
  forM_ files $ \file -> do

    when ("lhs" `isSuffixOf` file) $ do

      ghcResult <- runGhcUnlit (testdir </> file)
      myResult  <- runMyUnlit (testdir </> file)

      putStrLn $ "Testing file: " ++ file
      unless (myResult == ghcResult ++ "\n") $ do

        hPutStrLn stderr $ "Error in file: " ++ file
        hPutStrLn stderr $ ppDiff $ getGroupedDiff (breakAndMark ghcResult) (breakAndMark myResult)

  exitSuccess
