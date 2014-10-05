
{-# LANGUAGE GADTs, OverloadedStrings, CPP #-}
module Unlit.String
       (unlit, relit
       ,Style, all, infer, latex, bird, haskell, markdown, tildefence, backtickfence
       ,Lang, forLang, WhitespaceMode(..)) where
import Prelude hiding (all, or)
import Data.List (isPrefixOf, isInfixOf)
import Prelude hiding (all, or, replicate, drop, dropWhile, takeWhile, length, lines, unlines, getContents, putStrLn)
import Control.Monad (msum)
import Data.Char (isSpace)
import Data.Maybe (maybe, maybeToList, listToMaybe, fromMaybe)
import Data.Monoid (mempty,mappend)
import Data.String (IsString(..))

data Delim where
  LaTeX         :: BeginEnd -> Delim
  OrgMode       :: BeginEnd -> Maybe Lang -> Delim
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
  show (OrgMode Begin l) = "#+BEGIN_SRC" >#< maybe "" id l
  show (OrgMode End _)   = "#+END_SRC"
  show  Bird             = ">"
  show (TildeFence l)    = "~~~" >#< maybe "" id l
  show (BacktickFence l) = "```" >#< maybe "" id l

type Recogniser = String -> Maybe Delim

isLaTeX :: Recogniser
isLaTeX l
  | "\\begin{code}" `isPrefixOf` stripStart l = return $ LaTeX Begin
  | "\\end{code}"   `isPrefixOf` stripStart l = return $ LaTeX End
  | otherwise = Nothing

isOrgMode :: Maybe Lang -> Recogniser
isOrgMode lang l
  | "#+BEGIN_SRC" `isPrefixOf` stripStart l
    && maybe True (`isInfixOf` l) lang       = return $ OrgMode Begin lang
  | "#+END_SRC"   `isPrefixOf` stripStart l = return $ OrgMode End Nothing
  | otherwise = Nothing

stripStart :: String -> String
stripStart = dropWhile isSpace

isBird :: Recogniser
isBird l = (l == ">") || ("> " `isPrefixOf` l) ?: Bird

stripBird :: String -> String
stripBird = stripBird' KeepIndent

stripBird' :: WhitespaceMode -> String -> String
stripBird' KeepAll    l = " " `mappend` drop 1 l
stripBird' KeepIndent l =        drop 2 l

isTildeFence :: Maybe Lang -> Recogniser
isTildeFence lang l =
  if "~~~" `isPrefixOf` stripStart l then
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
  if "```" `isPrefixOf` stripStart l then
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
    go (OrgMode _ lang)     = isOrgMode lang l

match :: Delim -> Delim -> Bool
match (LaTeX Begin)     (LaTeX End)             = True
match (OrgMode Begin _) (OrgMode End _)         = True
match (TildeFence _)    (TildeFence Nothing)    = True
match (BacktickFence _) (BacktickFence Nothing) = True
match  _                 _                      = False

type Style = [Delim]

bird             = [Bird]
latex            = [LaTeX Begin, LaTeX End]
orgmode          = [OrgMode Begin Nothing, OrgMode End Nothing]
haskell          = latex ++ bird
tildefence       = [TildeFence Nothing]
backtickfence    = [BacktickFence Nothing]
markdown         = bird ++ tildefence ++ backtickfence
all              = latex ++ markdown
infer            = []

forLang :: Lang -> Style -> Style
forLang l = map (setLang (Just l))

setLang :: Maybe Lang -> Delim -> Delim
setLang lang (TildeFence _)       = TildeFence lang
setLang lang (BacktickFence _)    = BacktickFence lang
setLang lang (OrgMode beginEnd _) = OrgMode beginEnd lang
setLang _     d                   = d

doInfer :: Maybe Delim -> [Delim]
doInfer  Nothing             = []
doInfer (Just (LaTeX _))     = latex
doInfer (Just (OrgMode _ _)) = orgmode
doInfer (Just _)             = markdown

data WhitespaceMode where
  KeepIndent :: WhitespaceMode -- ^ keeps only indentations
  KeepAll    :: WhitespaceMode -- ^ keeps all lines and whitespace

or :: [a] -> [a] -> [a]
xs `or` [] = xs
[] `or` ys = ys
xs `or` ys = xs

unlit :: WhitespaceMode -> [Delim] -> String -> String
unlit ws ss = unlines . unlit' ws ss Nothing . zip [1..] . lines

type State = Maybe Delim

unlit' :: WhitespaceMode -> [Delim] -> State -> [(Int, String)] -> [String]
unlit' _ _ _ [] = []
unlit' ws ss q ((n, l):ls) = case (q, q') of


  (Nothing  , Nothing)                 -> continue $ lineIfKeepAll
  (Nothing  , Just Bird)               -> open     $ lineIfKeepIndent ++ [stripBird' ws l]
  (Just Bird, Just Bird)               -> continue $                     [stripBird' ws l]
  (Just Bird, Nothing)                 -> close    $ lineIfKeepAll
  (Nothing  , Just (LaTeX End))        -> spurious $ LaTeX End
  (Nothing  , Just (OrgMode End lang)) -> spurious $ OrgMode End lang
  (Nothing  , Just o)                  -> open     $ lineIfKeepAll ++ lineIfKeepIndent
  (Just o   , Nothing)                 -> continue $ return l
  (Just o   , Just Bird)               -> continue $ return l
  (Just o   , Just c)                  -> if not (o `match` c) then
                                       spurious c
                                     else
                                       close $ lineIfKeepAll
  where
    q'               = isDelim (ss `or` all) l
    continueWith q l = l ++ unlit' ws (ss `or` doInfer q') q ls
    open             = continueWith q'
    continue         = continueWith q
    close            = continueWith Nothing
    spurious       q = error ("at line " ++ show n ++ ": spurious " ++ show q)
    lineIfKeepAll, lineIfKeepIndent :: [String]
    lineIfKeepAll    = case ws of KeepAll    -> return mempty ; _ -> mempty
    lineIfKeepIndent = case ws of KeepIndent -> return mempty ; _ -> mempty

relit :: Style -> Style -> String -> String
relit ss ts = unlines . relit' ss (head ts) Nothing . zip [1..] . lines

emitBird :: String -> String
emitBird l = "> " `mappend` l

emitOpen :: Delim -> Maybe String -> [String]
emitOpen  Bird              l = mempty : map emitBird (maybeToList l)
emitOpen (LaTeX End)        l = emitOpen (LaTeX Begin) l
emitOpen (OrgMode End lang) l = emitOpen (OrgMode Begin lang) l
emitOpen  del               l = id (show del) : maybeToList l

emitCode :: Delim -> String -> String
emitCode Bird l = emitBird l
emitCode _    l = l

emitClose :: Delim -> String
emitClose  Bird                = mempty
emitClose (LaTeX Begin)        = emitClose (LaTeX End)
emitClose (OrgMode Begin lang) = emitClose (OrgMode End lang)
emitClose  del                 = id (show (setLang Nothing del))

relit' :: Style -> Delim -> State -> [(Int, String)] -> [String]
relit' _ _   Nothing    [] = []
relit' _ ts (Just Bird) [] = emitClose ts : []
relit' _ _  (Just o)    [] = error ("unexpected EOF; unmatched " ++ show o)
relit' ss ts q ((n, l):ls) = case (q, q') of

  (Nothing  , Nothing)                 -> l : continue
  (Nothing  , Just Bird)               -> blockOpen     $ Just (stripBird l)
  (Just Bird, Just Bird)               -> blockContinue $       stripBird l
  (Just Bird, Nothing)                 -> blockClose
  (Nothing  , Just (LaTeX End))        -> spurious (LaTeX End)
  (Nothing  , Just (OrgMode End lang)) -> spurious (OrgMode End lang)
  (Nothing  , Just o)                  -> blockOpen     $ Nothing
  (Just o   , Nothing)                 -> blockContinue $ l
  (Just o   , Just Bird)               -> l : continue
  (Just o   , Just c)                  -> if o `match` c then blockClose else spurious c

  where
    q'              = isDelim (ss `or` all) l
    continueWith  q = relit' (ss `or` doInfer q') ts q ls
    continue        = continueWith q
    blockOpen     l = emitOpen  ts l ++ continueWith q'
    blockContinue l = emitCode  ts l : continue
    blockClose      = emitClose ts   : continueWith Nothing
    spurious      q = error ("at line " ++ show n ++ ": spurious " ++ show q)

infixr 1 ?:
infixr 5 >#<

(?:) :: Bool -> a -> Maybe a
True  ?: x = Just x
False ?: _ = Nothing

(>#<) :: String -> String -> String
"" >#< y  = y
x  >#< "" = x
x  >#< y  = x ++ " " ++ y

