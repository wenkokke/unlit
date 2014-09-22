{-# LANGUAGE GADTs, OverloadedStrings #-}
module Unlit.String
       (unlit, relit
       ,Style, all, latex, bird, haskell, markdown, tildefence, backtickfence
       ,Lang, forLang) where

import Prelude hiding (all, or)
import Data.List (isPrefixOf, isInfixOf)
import Control.Monad (msum)
import Data.Char (isSpace)
import Data.Maybe (maybe, maybeToList, listToMaybe, fromMaybe)
import Data.Monoid (mempty,(<>))

data Delim where
  LaTeX         :: BeginEnd -> Delim
  Bird          :: Delim
  TildeFence    :: Maybe Lang -> Delim
  BacktickFence :: Maybe Lang -> Delim
  deriving (Eq)

data BeginEnd where
  Begin :: BeginEnd
  End   :: BeginEnd
  deriving (Eq)

type Lang = String

instance Show Delim where
  show (LaTeX Begin)     = "\\begin{code}"
  show (LaTeX End)       = "\\end{code}"
  show  Bird             = ">"
  show (TildeFence l)    = "~~~" ++ (maybe "" id l)
  show (BacktickFence l) = "```" ++ (maybe "" id l)

type Recogniser = String -> Maybe Delim

infix 1 ?:

(?:) :: Bool -> a -> Maybe a
True  ?: x = Just x
False ?: _ = Nothing

isLaTeX :: Recogniser
isLaTeX l
  | "\\begin{code}" `isPrefixOf` lstrip l = return $ LaTeX Begin
  | "\\end{code}"   `isPrefixOf` lstrip l = return $ LaTeX End
  | otherwise = Nothing

lstrip :: String -> String
lstrip = dropWhile isSpace

isBird :: Recogniser
isBird l = (l == ">") || ("> " `isPrefixOf` l) ?: Bird

stripBird :: String -> String
stripBird l = drop 2 l

isTildeFence :: Maybe Lang -> Recogniser
isTildeFence lang l =
  if "~~~" `isPrefixOf` lstrip l then
    (
      if maybe True (`isInfixOf` l) lang then
        return $ TildeFence lang
      else
        return $ TildeFence Nothing
    )
  else
    Nothing

isBacktickFence :: Maybe Lang -> Recogniser
isBacktickFence lang l =
  if "```" `isPrefixOf` lstrip l then
    (
      if maybe True (`isInfixOf` l) lang then
        return $ TildeFence lang
      else
        return $ TildeFence Nothing
    )
  else
    Nothing

isDelim :: [Delim] -> Recogniser
isDelim ds l = msum (map go ds)
  where
    go :: Delim -> Maybe Delim
    go (LaTeX _)            = isLaTeX l
    go  Bird                = isBird l
    go (TildeFence lang)    = isTildeFence lang l
    go (BacktickFence lang) = isBacktickFence lang l

match :: Delim -> Delim -> Bool
match (LaTeX Begin)     (LaTeX End)             = True
match (TildeFence _)    (TildeFence Nothing)    = True
match (BacktickFence _) (BacktickFence Nothing) = True
match  _                 _                      = False

type Style = [Delim]

bird             = [Bird]
latex            = [LaTeX Begin, LaTeX End]
haskell          = latex ++ bird
tildefence       = [TildeFence Nothing]
backtickfence    = [BacktickFence Nothing]
markdown         = bird ++ tildefence ++ backtickfence
all              = latex ++ markdown
infer            = []

forLang :: Lang -> Style -> Style
forLang l = map (setLang (Just l))

setLang :: Maybe Lang -> Delim -> Delim
setLang l (TildeFence _)    = TildeFence l
setLang l (BacktickFence _) = BacktickFence l
setLang _  d                = d

doInfer :: Maybe Delim -> [Delim]
doInfer  Nothing         = []
doInfer (Just (LaTeX _)) = latex
doInfer (Just _)         = markdown

or :: [a] -> [a] -> [a]
xs `or` [] = xs
[] `or` ys = ys
xs `or` ys = xs

unlit :: [Delim] -> String -> String
unlit ss = unlines . unlit' ss Nothing 0 . zip [1..] . lines

type State = Maybe Delim

countSpaces :: String -> Int
countSpaces = length . takeWhile isSpace

unlit' :: [Delim] -> State -> Int -> [(Int, String)] -> [String]
unlit' _ _ _ [] = []
unlit' ss q ws ((n, l):ls) = case (q, q') of


  (Nothing  , Nothing)          -> continue
  (Nothing  , Just Bird)        -> blockOpen     $ Just (stripBird l)
  (Just Bird, Just Bird)        -> blockContinue $ stripBird l
  (Just Bird, Nothing)          -> blockClose
  (Nothing  , Just (LaTeX End)) -> spurious (LaTeX End)
  (Nothing  , Just o)           -> blockOpen     $ Nothing
  (Just o   , Nothing)          -> blockContinue $ l
  (Just o   , Just c)           -> if o `match` c then blockClose else spurious c

  where
    q'                = isDelim (ss `or` all) l
    continueWith q ws = unlit' (ss `or` doInfer q') q ws ls
    continue          = continueWith q ws
    blockOpen       l = maybeToList l ++ continueWith q' ws
    blockContinue   l = l : continue
    blockClose        = mempty : continueWith Nothing ws
    spurious        q = error ("at line " ++ show n ++ ": spurious " ++ show q)

relit :: Style -> Style -> String -> String
relit ss ts = unlines . relit' ss (head ts) Nothing . zip [1..] . lines

emitBird :: String -> String
emitBird l = "> " <> l

emitOpen :: Delim -> Maybe String -> [String]
emitOpen  Bird       l = mempty : map emitBird (maybeToList l)
emitOpen (LaTeX End) l = emitOpen (LaTeX Begin) l
emitOpen  del        l = id (show del) : maybeToList l

emitCode :: Delim -> String -> String
emitCode Bird l = emitBird l
emitCode _    l = l

emitClose :: Delim -> String
emitClose  Bird         = mempty
emitClose (LaTeX Begin) = emitClose (LaTeX End)
emitClose  del          = id (show (setLang Nothing del))

relit' :: Style -> Delim -> State -> [(Int, String)] -> [String]
relit' _ _ _ [] = []
relit' ss ts q ((n, l):ls) = case (q, q') of

  (Nothing  , Nothing)          -> l : continue
  (Nothing  , Just Bird)        -> blockOpen     $ Just (stripBird l)
  (Just Bird, Just Bird)        -> blockContinue $ stripBird l
  (Just Bird, Nothing)          -> blockClose
  (Nothing  , Just (LaTeX End)) -> spurious (LaTeX End)
  (Nothing  , Just o)           -> blockOpen     $ Nothing
  (Just o   , Nothing)          -> blockContinue $ l
  (Just o   , Just c)           -> if o `match` c then blockClose else spurious c

  where
    q'              = isDelim (ss `or` all) l
    continueWith  q = relit' (ss `or` doInfer q') ts q ls
    continue        = continueWith q
    blockOpen     l = emitOpen  ts l ++ continueWith q'
    blockContinue l = emitCode  ts l : continue
    blockClose      = emitClose ts   : continueWith Nothing
    spurious      q = error ("at line " ++ show n ++ ": spurious " ++ show q)

