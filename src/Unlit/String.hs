
{-# LANGUAGE OverloadedStrings, CPP #-}
module Unlit.String (
  unlit, relit
  , Style, all, infer, latex, bird, jekyll,  haskell, markdown, tildefence, backtickfence
  , Lang, forLang, WhitespaceMode(..)
) where

import Data.Foldable (asum)
import Data.Bool (bool)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Monoid ((<>))
import Prelude hiding (all, or)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, dropWhileEnd)
import Data.Char (isSpace)
stripStart, stripEnd :: String -> String
stripStart = dropWhile isSpace
stripEnd = dropWhileEnd isSpace

data Delim
  = LaTeX         BeginEnd
  | OrgMode       BeginEnd (Maybe Lang)
  | Bird
  | Jekyll        BeginEnd (Maybe Lang)
  | TildeFence    (Maybe Lang)
  | BacktickFence (Maybe Lang)
  deriving (Eq)

data BeginEnd
  = Begin
  | End
  deriving (Eq)

type Lang = String

emitDelim :: Delim -> String
emitDelim (LaTeX Begin)     = "\\begin{code}"
emitDelim (LaTeX End)       = "\\end{code}"
emitDelim (OrgMode Begin l) = "#+BEGIN_SRC" >#< fromMaybe "" l
emitDelim (OrgMode End _)   = "#+END_SRC"
emitDelim  Bird             = ">"
emitDelim (Jekyll Begin l)  = "{% highlight " >#< fromMaybe "" l >#< " %}"
emitDelim (Jekyll End   _)  = "{% endhighlight %}"
emitDelim (TildeFence l)    = "~~~" >#< fromMaybe "" l
emitDelim (BacktickFence l) = "```" >#< fromMaybe "" l

type Recogniser = String -> Maybe Delim

isLaTeX :: Recogniser
isLaTeX l
  | "\\begin{code}" `isPrefixOf` stripStart l = Just $ LaTeX Begin
  | "\\end{code}"   `isPrefixOf` stripStart l = Just $ LaTeX End
  | otherwise = Nothing

isOrgMode :: Maybe Lang -> Recogniser
isOrgMode lang l
  | "#+BEGIN_SRC" `isPrefixOf` stripStart l
    && maybe True (`isInfixOf` l) lang      = Just $ OrgMode Begin lang
  | "#+END_SRC"   `isPrefixOf` stripStart l = Just $ OrgMode End Nothing
  | otherwise = Nothing

isBird :: Recogniser
isBird l = bool Nothing (Just Bird) (l == ">" || "> " `isPrefixOf` l)

stripBird :: String -> String
stripBird = stripBird' KeepIndent

stripBird' :: WhitespaceMode -> String -> String
stripBird' KeepAll    l = " " <> drop 1 l
stripBird' KeepIndent l = drop 2 l

isJekyll :: Maybe Lang -> Recogniser
isJekyll lang l
  | "{% highlight" `isPrefixOf` stripStart l
    && maybe True (`isInfixOf` l) lang
    && "%}" `isSuffixOf` stripEnd l     = Just $ Jekyll Begin lang
  | "{% endhighlight %}" `isPrefixOf` l = Just $ Jekyll End   lang
  | otherwise                           = Nothing

isTildeFence :: Maybe Lang -> Recogniser
isTildeFence lang l =
  if "~~~" `isPrefixOf` stripStart l then
    if maybe True (`isInfixOf` l) lang then
      Just $ TildeFence lang
    else
      Just $ TildeFence Nothing
  else
    Nothing

isBacktickFence :: Maybe Lang -> Recogniser
isBacktickFence lang l =
  if "```" `isPrefixOf` stripStart l then
    if maybe True (`isInfixOf` l) lang then
      Just $ TildeFence lang
    else
      Just $ TildeFence Nothing
  else
    Nothing

isDelim :: Style -> Recogniser
isDelim ds l = asum (map go ds)
  where
    go (LaTeX _)            = isLaTeX l
    go  Bird                = isBird l
    go (Jekyll _ lang)      = isJekyll lang l
    go (TildeFence lang)    = isTildeFence lang l
    go (BacktickFence lang) = isBacktickFence lang l
    go (OrgMode _ lang)     = isOrgMode lang l

match :: Delim -> Delim -> Bool
match (LaTeX Begin)     (LaTeX End)             = True
match (Jekyll Begin _)  (Jekyll End _)          = True
match (OrgMode Begin _) (OrgMode End _)         = True
match (TildeFence _)    (TildeFence Nothing)    = True
match (BacktickFence _) (BacktickFence Nothing) = True
match  _                 _                      = False

type Style = [Delim]

bird, latex, orgmode, haskell, jekyll, tildefence, backtickfence, markdown, all, infer :: Style
bird             = [Bird]
latex            = [LaTeX Begin, LaTeX End]
orgmode          = [OrgMode Begin Nothing, OrgMode End Nothing]
haskell          = latex <> bird
jekyll           = [Jekyll Begin Nothing, Jekyll End Nothing]
tildefence       = [TildeFence Nothing]
backtickfence    = [BacktickFence Nothing]
markdown         = bird <> tildefence <> backtickfence
all              = latex <> markdown
infer            = []

forLang :: Lang -> Style -> Style
forLang = map . setLang . Just

setLang :: Maybe Lang -> Delim -> Delim
setLang lang (TildeFence _)       = TildeFence lang
setLang lang (BacktickFence _)    = BacktickFence lang
setLang lang (OrgMode beginEnd _) = OrgMode beginEnd lang
setLang lang (Jekyll beginEnd _)  = Jekyll beginEnd lang
setLang _     d                   = d

doInfer :: Maybe Delim -> Style
doInfer  Nothing             = []
doInfer (Just (LaTeX _))     = latex
doInfer (Just (Jekyll _ _))  = jekyll
doInfer (Just (OrgMode _ _)) = orgmode
doInfer (Just _)             = markdown

data WhitespaceMode
  = KeepIndent -- ^ keeps only indentations
  | KeepAll    -- ^ keeps all lines and whitespace

or :: [a] -> [a] -> [a]
xs `or` [] = xs
[] `or` ys = ys
xs `or` _  = xs

unlit :: WhitespaceMode -> Style -> String -> String
unlit ws ss = unlines . unlit' ws ss Nothing . zip [1..] . lines

type State = Maybe Delim

unlit' :: WhitespaceMode -> Style -> State -> [(Int, String)] -> [String]
unlit' _ _ _ [] = []
unlit' ws ss q ((n, l):ls) = case (q, q') of


  (Nothing  , Nothing)                 -> continue   $ lineIfKeepAll
  (Nothing  , Just Bird)               -> open       $ lineIfKeepIndent <> [stripBird' ws l]
  (Just Bird, Just Bird)               -> continue   $                     [stripBird' ws l]
  (Just Bird, Nothing)                 -> close      $ lineIfKeepAll
  (Nothing  , Just (LaTeX End))        -> spurious n $ LaTeX End
  (Nothing  , Just (Jekyll End lang))  -> spurious n $ Jekyll End lang
  (Nothing  , Just (OrgMode End lang)) -> spurious n $ OrgMode End lang
  (Nothing  , Just _o)                 -> open       $ lineIfKeepAll <> lineIfKeepIndent
  (Just _o  , Nothing)                 -> continue   $ [l]
  (Just _o  , Just Bird)               -> continue   $ [l]
  (Just o   , Just c)                  -> if not (o `match` c) then
                                       spurious n c
                                     else
                                       close $ lineIfKeepAll
  where
    q'                = isDelim (ss `or` all) l
    continueWith r l' = l' <> unlit' ws (ss `or` doInfer q') r ls
    open              = continueWith q'
    continue          = continueWith q
    close             = continueWith Nothing
    lineIfKeepAll     = case ws of KeepAll    -> [""]; _ -> []
    lineIfKeepIndent  = case ws of KeepIndent -> [""]; _ -> []

relit :: Style -> Style -> String -> String
relit ss ts = unlines . relit' ss (head ts) Nothing . zip [1..] . lines

emitBird :: String -> String
emitBird l = "> " <> l

emitOpen :: Delim -> Maybe String -> [String]
emitOpen  Bird              l = "" : map emitBird (maybeToList l)
emitOpen (LaTeX End)        l = emitOpen (LaTeX Begin) l
emitOpen (Jekyll End lang)  l = emitOpen (Jekyll Begin lang) l
emitOpen (OrgMode End lang) l = emitOpen (OrgMode Begin lang) l
emitOpen  del               l = emitDelim del : maybeToList l

emitCode :: Delim -> String -> String
emitCode Bird l = emitBird l
emitCode _    l = l

emitClose :: Delim -> String
emitClose  Bird                = ""
emitClose (LaTeX Begin)        = emitClose (LaTeX End)
emitClose (Jekyll Begin lang)  = emitClose (Jekyll End lang)
emitClose (OrgMode Begin lang) = emitClose (OrgMode End lang)
emitClose  del                 = emitDelim (setLang Nothing del)

relit' :: Style -> Delim -> State -> [(Int, String)] -> [String]
relit' _ _   Nothing    [] = []
relit' _ ts (Just Bird) [] = emitClose ts : []
relit' _ _  (Just o)    [] = eof o
relit' ss ts q ((n, l):ls) = case (q, q') of

  (Nothing  , Nothing)                 -> l : continue
  (Nothing  , Just Bird)               -> blockOpen     $ Just (stripBird l)
  (Just Bird, Just Bird)               -> blockContinue $       stripBird l
  (Just Bird, Nothing)                 -> blockClose
  (Nothing  , Just (LaTeX End))        -> spurious n    $ LaTeX End
  (Nothing  , Just (Jekyll End lang))  -> spurious n    $ Jekyll End lang
  (Nothing  , Just (OrgMode End lang)) -> spurious n    $ OrgMode End lang
  (Nothing  , Just _o)                 -> blockOpen     $ Nothing
  (Just _o  , Nothing)                 -> blockContinue $ l
  (Just _o  , Just Bird)               -> l : continue
  (Just o   , Just c)                  -> if o `match` c then blockClose else spurious n c

  where
    q'               = isDelim (ss `or` all) l
    continueWith  r  = relit' (ss `or` doInfer q') ts r ls
    continue         = continueWith q
    blockOpen     l' = emitOpen  ts l' <> continueWith q'
    blockContinue l' = emitCode  ts l' : continue
    blockClose       = emitClose ts   : continueWith Nothing

infixr 5 >#<

(>#<) :: String -> String -> String
"" >#< y  = y
x  >#< "" = x
x  >#< y  = x <> " " <> y

eof :: Delim -> a
eof q = error $ "unexpected EOF; unmatched " <>  (emitDelim q)

spurious :: Int -> Delim -> a
spurious n q = error $ "at line " <> show n <> ": spurious " <>  (emitDelim q)

