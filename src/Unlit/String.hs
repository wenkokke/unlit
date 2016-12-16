module Unlit.String (
  unlit, relit
  , Style, all, infer, latex, bird, jekyll,  haskell, markdown, tildefence, backtickfence
  , Lang, setLang, WhitespaceMode(..)
  , Error(..), showError
) where

import Data.Functor ((<$>))
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

data Delimiter
  = LaTeX         BeginEnd
  | OrgMode       BeginEnd Lang
  | Bird
  | Jekyll        BeginEnd Lang
  | TildeFence    Lang
  | BacktickFence Lang
  deriving (Eq, Show)

data BeginEnd
  = Begin
  | End
  deriving (Eq, Show)

isBegin :: Delimiter -> Bool
isBegin (LaTeX   x  ) = x == Begin
isBegin (OrgMode x _) = x == Begin
isBegin (Jekyll  x _) = x == Begin
isBegin  _            = False

type Lang = Maybe String

emitDelimiter :: Delimiter -> String
emitDelimiter (LaTeX Begin)     = "\\begin{code}"
emitDelimiter (LaTeX End)       = "\\end{code}"
emitDelimiter (OrgMode Begin l) = "#+BEGIN_SRC" <+> fromMaybe "" l
emitDelimiter (OrgMode End _)   = "#+END_SRC"
emitDelimiter  Bird             = ">"
emitDelimiter (Jekyll Begin l)  = "{% highlight " <+> fromMaybe "" l <+> " %}"
emitDelimiter (Jekyll End   _)  = "{% endhighlight %}"
emitDelimiter (TildeFence l)    = "~~~" <+> fromMaybe "" l
emitDelimiter (BacktickFence l) = "```" <+> fromMaybe "" l

type Recogniser = String -> Maybe Delimiter

isLaTeX :: Recogniser
isLaTeX l
  | "\\begin{code}" `isPrefixOf` stripStart l = Just $ LaTeX Begin
  | "\\end{code}"   `isPrefixOf` stripStart l = Just $ LaTeX End
  | otherwise = Nothing

isOrgMode :: Lang -> Recogniser
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

isJekyll :: Lang -> Recogniser
isJekyll lang l
  | "{% highlight" `isPrefixOf` stripStart l
    && maybe True (`isInfixOf` l) lang
    && "%}" `isSuffixOf` stripEnd l     = Just $ Jekyll Begin lang
  | "{% endhighlight %}" `isPrefixOf` l = Just $ Jekyll End   lang
  | otherwise                           = Nothing

isTildeFence :: Lang -> Recogniser
isTildeFence lang l
  | "~~~" `isPrefixOf` stripStart l =
    Just $ TildeFence $
      if maybe True (`isInfixOf` l) lang then
        lang
      else
        Nothing
  | otherwise = Nothing

isBacktickFence :: Lang -> Recogniser
isBacktickFence lang l
  | "```" `isPrefixOf` stripStart l =
    Just $ TildeFence $
      if maybe True (`isInfixOf` l) lang then
        lang
       else
         Nothing
  | otherwise = Nothing

isDelimiter :: Style -> Recogniser
isDelimiter ds l = asum (map go ds)
  where
    go (LaTeX _)            = isLaTeX l
    go  Bird                = isBird l
    go (Jekyll _ lang)      = isJekyll lang l
    go (TildeFence lang)    = isTildeFence lang l
    go (BacktickFence lang) = isBacktickFence lang l
    go (OrgMode _ lang)     = isOrgMode lang l

match :: Delimiter -> Delimiter -> Bool
match (LaTeX Begin)     (LaTeX End)             = True
match (Jekyll Begin _)  (Jekyll End _)          = True
match (OrgMode Begin _) (OrgMode End _)         = True
match (TildeFence _)    (TildeFence Nothing)    = True
match (BacktickFence _) (BacktickFence Nothing) = True
match  _                 _                      = False

type Style = [Delimiter]

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

setLang :: Lang -> Style -> Style
setLang = map . setLang'

setLang' :: Lang -> Delimiter -> Delimiter
setLang' lang (TildeFence _)       = TildeFence lang
setLang' lang (BacktickFence _)    = BacktickFence lang
setLang' lang (OrgMode beginEnd _) = OrgMode beginEnd lang
setLang' lang (Jekyll beginEnd _)  = Jekyll beginEnd lang
setLang' _     d                   = d

inferred :: Maybe Delimiter -> Style
inferred  Nothing             = []
inferred (Just (LaTeX _))     = latex
inferred (Just (Jekyll _ _))  = jekyll
inferred (Just (OrgMode _ _)) = orgmode
inferred (Just _)             = markdown

data WhitespaceMode
  = KeepIndent -- ^ keeps only indentations
  | KeepAll    -- ^ keeps all lines and whitespace

or :: [a] -> [a] -> [a]
xs `or` [] = xs
[] `or` ys = ys
xs `or` _  = xs

unlit :: WhitespaceMode -> Style -> String -> Either Error String
unlit ws ss = fmap unlines . unlit' ws ss Nothing . zip [1..] . lines

type State = Maybe Delimiter

unlit' :: WhitespaceMode -> Style -> State -> [(Int, String)] -> Either Error [String]
unlit' _ _ _ [] = Right []
unlit' ws ss q ((n, l):ls) = case (q, q') of

  (Nothing  , Nothing)                 -> continue   $ lineIfKeepAll
  (Nothing  , Just Bird)               -> open       $ lineIfKeepIndent <> [stripBird' ws l]
  (Just Bird, Just Bird)               -> continue   $                     [stripBird' ws l]
  (Just Bird, Nothing)                 -> close      $ lineIfKeepAll
  (Nothing  , Just c)                  -> if isBegin c then open $ lineIfKeepAll <> lineIfKeepIndent else Left $ SpuriousDelimiter n c
  (Just _o  , Nothing)                 -> continue   $ [l]
  (Just _o  , Just Bird)               -> continue   $ [l]
  (Just o   , Just c)                  -> if o `match` c then close $ lineIfKeepAll else Left $ SpuriousDelimiter n c

  where
    q'                = isDelimiter (ss `or` all) l
    continueWith r l' = (l' <>) <$> unlit' ws (ss `or` inferred q') r ls
    open              = continueWith q'
    continue          = continueWith q
    close             = continueWith Nothing
    lineIfKeepAll     = case ws of KeepAll    -> [""]; _ -> []
    lineIfKeepIndent  = case ws of KeepIndent -> [""]; _ -> []

relit :: Style -> Style -> String -> Either Error String
relit ss ts = fmap unlines . relit' ss (head ts) Nothing . zip [1..] . lines

emitBird :: String -> String
emitBird l = "> " <> l

emitOpen :: Delimiter -> Maybe String -> [String]
emitOpen  Bird              l = "" : map emitBird (maybeToList l)
emitOpen (LaTeX End)        l = emitOpen (LaTeX Begin) l
emitOpen (Jekyll End lang)  l = emitOpen (Jekyll Begin lang) l
emitOpen (OrgMode End lang) l = emitOpen (OrgMode Begin lang) l
emitOpen  del               l = emitDelimiter del : maybeToList l

emitCode :: Delimiter -> String -> String
emitCode Bird l = emitBird l
emitCode _    l = l

emitClose :: Delimiter -> String
emitClose  Bird                = ""
emitClose (LaTeX Begin)        = emitClose (LaTeX End)
emitClose (Jekyll Begin lang)  = emitClose (Jekyll End lang)
emitClose (OrgMode Begin lang) = emitClose (OrgMode End lang)
emitClose  del                 = emitDelimiter (setLang' Nothing del)

relit' :: Style -> Delimiter -> State -> [(Int, String)] -> Either Error [String]
relit' _ _   Nothing    [] = Right []
relit' _ ts (Just Bird) [] = Right $ emitClose ts : []
relit' _ _  (Just o)    [] = Left $ UnexpectedEnd o
relit' ss ts q ((n, l):ls) = case (q, q') of

  (Nothing  , Nothing)                 -> (l :) <$> continue
  (Nothing  , Just Bird)               -> blockOpen     $ Just (stripBird l)
  (Just Bird, Just Bird)               -> blockContinue $       stripBird l
  (Just Bird, Nothing)                 -> blockClose
  (Nothing  , Just c)                  -> if isBegin c then blockOpen Nothing else Left $ SpuriousDelimiter n c
  (Just _o  , Nothing)                 -> blockContinue $ l
  (Just _o  , Just Bird)               -> (l :) <$> continue
  (Just o   , Just c)                  -> if o `match` c then blockClose else Left $ SpuriousDelimiter n c

  where
    q'               = isDelimiter (ss `or` all) l
    continueWith  r  = relit' (ss `or` inferred q') ts r ls
    continue         = continueWith q
    blockOpen     l' = (emitOpen  ts l' <>) <$> continueWith q'
    blockContinue l' = (emitCode  ts l' :)  <$> continue
    blockClose       = (emitClose ts    :)  <$> continueWith Nothing

data Error
  = SpuriousDelimiter Int Delimiter
  | UnexpectedEnd     Delimiter
  deriving (Eq, Show)

showError :: Error -> String
showError (UnexpectedEnd q) = "unexpected EOF; unmatched " <> emitDelimiter q
showError (SpuriousDelimiter n q) = "at line " <>  (show n) <> ": spurious " <> emitDelimiter q

infixr 5 <+>

(<+>) :: String -> String -> String
"" <+> y  = y
x  <+> "" = x
x  <+> y  = x <> " " <> y

