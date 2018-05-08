module Unlit.String (
  unlit, relit
  , Style, parseStyle
  , WhitespaceMode(..), parseWhitespaceMode
  , all, infer, latex, bird, jekyll,  haskell, markdown, tildefence, backtickfence
  , Lang, setLang
  , Error(..), showError
) where

import Data.Functor ((<$>))
import Data.Foldable (asum)
import Data.Bool (bool)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Monoid ((<>))
import Prelude hiding (all, or)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, dropWhileEnd)
import qualified Data.Char as Char

stripStart, stripEnd, toLower :: String -> String
stripStart = dropWhile Char.isSpace
stripEnd   = dropWhileEnd Char.isSpace
toLower    = map Char.toLower

data Delimiter
  = LaTeX    BeginEnd
  | OrgMode  BeginEnd Lang
  | Bird
  | Jekyll   BeginEnd Lang
  | Markdown Fence Lang
  | Asciidoc BeginEnd Lang
  deriving (Eq, Show)

data BeginEnd
  = Begin
  | End
  deriving (Eq, Show)

isBegin :: Delimiter -> Bool
isBegin (LaTeX    Begin  ) = True
isBegin (OrgMode  Begin _) = True
isBegin (Jekyll   Begin _) = True
isBegin (Asciidoc Begin _) = True
isBegin (Markdown _ _)     = True
isBegin  _                 = False

setBegin :: BeginEnd -> Delimiter -> Delimiter
setBegin beginEnd (LaTeX    _  )    = LaTeX    beginEnd
setBegin beginEnd (OrgMode  _ lang) = OrgMode  beginEnd lang
setBegin beginEnd (Jekyll   _ lang) = Jekyll   beginEnd lang
setBegin beginEnd (Asciidoc _ lang) = Asciidoc beginEnd lang
setBegin _         del              = del

data Fence
  = Tilde
  | Backtick
  deriving (Eq, Show)

type Lang = Maybe String

containsLang :: String -> Lang -> Bool
containsLang _ Nothing     = True
containsLang l (Just lang) = toLower lang `isInfixOf` toLower l

emitDelimiter :: Delimiter -> String
emitDelimiter (LaTeX Begin)         = "\\begin{code}"
emitDelimiter (LaTeX End)           = "\\end{code}"
emitDelimiter (OrgMode Begin l)     = "#+BEGIN_SRC" <+> fromMaybe "" l
emitDelimiter (OrgMode End _)       = "#+END_SRC"
emitDelimiter  Bird                 = ">"
emitDelimiter (Jekyll Begin l)      = "{% highlight" <+> fromMaybe "" l <+> "%}"
emitDelimiter (Jekyll End   _)      = "{% endhighlight %}"
emitDelimiter (Asciidoc Begin l)    = "[source" <> maybe "" (", "<>) l <> "]\n----"
emitDelimiter (Asciidoc End   _)    = "----"
emitDelimiter (Markdown Tilde l)    = "~~~" <+> fromMaybe "" l
emitDelimiter (Markdown Backtick l) = "```" <+> fromMaybe "" l

infixr 5 <+>
(<+>) :: String -> String -> String
"" <+> y  = y
x  <+> "" = x
x  <+> y  = x <> " " <> y

type Recogniser = String -> Maybe Delimiter

isLaTeX :: Recogniser
isLaTeX l
  | "\\begin{code}" `isPrefixOf` stripStart l = Just $ LaTeX Begin
  | "\\end{code}"   `isPrefixOf` stripStart l = Just $ LaTeX End
  | otherwise = Nothing

isOrgMode :: Lang -> Recogniser
isOrgMode lang l
  | "#+BEGIN_SRC" `isPrefixOf` stripStart l
    && l `containsLang` lang                = Just $ OrgMode Begin lang
  | "#+END_SRC"   `isPrefixOf` stripStart l = Just $ OrgMode End Nothing
  | otherwise = Nothing

isBird :: Recogniser
isBird l = bool Nothing (Just Bird) (l == ">" || "> " `isPrefixOf` l)

stripBird :: String -> String
stripBird = stripBird' WsKeepIndent

stripBird' :: WhitespaceMode -> String -> String
stripBird' WsKeepAll    l = " " <> drop 1 l
stripBird' WsKeepIndent l = drop 2 l

isJekyll :: Lang -> Recogniser
isJekyll lang l
  | "{% highlight" `isPrefixOf` stripStart l
    && l `containsLang` lang
    && "%}" `isSuffixOf` stripEnd l     = Just $ Jekyll Begin lang
  | "{% endhighlight %}" `isPrefixOf` l = Just $ Jekyll End   lang
  | otherwise                           = Nothing

isMarkdown :: Fence -> String -> Lang -> Recogniser
isMarkdown fence fenceStr lang l
  | fenceStr `isPrefixOf` stripStart l =
    Just $ Markdown fence $ bool Nothing lang (l `containsLang` lang)
  | otherwise = Nothing

isAsciidoc :: Lang -> Recogniser
isAsciidoc lang l
  | "[source" `isPrefixOf` l
    && l `containsLang` lang
    && "]" `isSuffixOf` stripEnd l = Just $ Asciidoc Begin lang
  | "----" `isPrefixOf` l          = Just $ Asciidoc End   lang
  | otherwise                      = Nothing

asciidocFence :: [(Int,String)] -> Maybe [(Int,String)]
asciidocFence ls | ((_,"----"):ls') <- ls = Just ls'
                 | otherwise              = Nothing

isDelimiter :: Style -> Recogniser
isDelimiter ds l = asum (map go ds)
  where
    go (LaTeX _)                = isLaTeX l
    go  Bird                    = isBird l
    go (Jekyll _ lang)          = isJekyll lang l
    go (Markdown Tilde lang)    = isMarkdown Tilde "~~~" lang l
    go (Markdown Backtick lang) = isMarkdown Backtick "```" lang l
    go (OrgMode _ lang)         = isOrgMode lang l
    go (Asciidoc _ lang)        = isAsciidoc lang l

match :: Delimiter -> Delimiter -> Bool
match (LaTeX Begin)      (LaTeX End)          = True
match (Jekyll Begin _)   (Jekyll End _)       = True
match (OrgMode Begin _)  (OrgMode End _)      = True
match (Asciidoc Begin _) (Asciidoc End _)     = True
match (Markdown f _)     (Markdown g Nothing) = f == g
match  _                  _                   = False

type Style = [Delimiter]

all, backtickfence, tildefence, bird, haskell, infer, jekyll, latex, markdown, orgmode, asciidoc :: Style
all           = latex <> markdown <> orgmode <> jekyll <> asciidoc
backtickfence = [Markdown Backtick Nothing]
tildefence    = [Markdown Tilde Nothing]
bird          = [Bird]
haskell       = latex <> bird
infer         = []
jekyll        = [Jekyll Begin Nothing, Jekyll End Nothing]
latex         = [LaTeX Begin, LaTeX End]
markdown      = bird <> tildefence <> backtickfence
orgmode       = [OrgMode Begin Nothing, OrgMode End Nothing]
asciidoc      = [Asciidoc Begin Nothing, Asciidoc End Nothing]

parseStyle :: String -> Maybe Style
parseStyle s = case toLower s of
  "all"           -> Just all
  "backtickfence" -> Just backtickfence
  "bird"          -> Just bird
  "haskell"       -> Just haskell
  "infer"         -> Just infer
  "jekyll"        -> Just jekyll
  "latex"         -> Just latex
  "markdown"      -> Just markdown
  "orgmode"       -> Just orgmode
  "asciidoc"      -> Just asciidoc
  "tildefence"    -> Just tildefence
  _               -> Nothing

setLang :: Lang -> Style -> Style
setLang = fmap . setLang'

setLang' :: Lang -> Delimiter -> Delimiter
setLang' lang (Markdown fence _)   = Markdown fence lang
setLang' lang (OrgMode beginEnd _) = OrgMode beginEnd lang
setLang' lang (Jekyll beginEnd _)  = Jekyll beginEnd lang
setLang' _     d                   = d

inferred :: Maybe Delimiter -> Style
inferred  Nothing              = []
inferred (Just (LaTeX _))      = latex
inferred (Just (Jekyll _ _))   = jekyll
inferred (Just (OrgMode _ _))  = orgmode
inferred (Just (Asciidoc _ _)) = asciidoc
inferred (Just _)              = markdown

data WhitespaceMode
  = WsKeepIndent -- ^ keeps only indentations
  | WsKeepAll    -- ^ keeps all lines and whitespace

parseWhitespaceMode :: String -> Maybe WhitespaceMode
parseWhitespaceMode s = case toLower s of
  "all"    -> Just WsKeepAll
  "indent" -> Just WsKeepIndent
  _        -> Nothing

or :: [a] -> [a] -> [a]
xs `or` [] = xs
[] `or` ys = ys
xs `or` _  = xs

unlit :: WhitespaceMode -> Style -> String -> Either Error String
unlit ws ss = fmap unlines . unlit' ws ss Nothing . zip [1..] . lines

type State = Maybe Delimiter

unlit' :: WhitespaceMode -> Style -> State -> [(Int, String)] -> Either Error [String]
unlit' _ _  Nothing    []  = Right []
unlit' _ _ (Just Bird) []  = Right []
unlit' _ _ (Just o)    []  = Left $ UnexpectedEnd o
unlit' ws ss q ((n, l):ls) = case (q, q') of

  (Nothing  , Nothing)   -> continue  $ lineIfKeepAll

  (Just Bird, Nothing)   -> close     $ lineIfKeepAll
  (Just _o  , Nothing)   -> continue  $ [l]

  (Nothing  , Just Bird) -> open      $ lineIfKeepIndent <> [stripBird' ws l]
  (Nothing  , Just (Asciidoc Begin _))
    | Just ls' <- asciidocFence ls
                         -> open' ls' $ lineIfKeepAll <> lineIfKeepIndent
  (Nothing  , Just c)
    | isBegin c          -> open      $ lineIfKeepAll <> lineIfKeepIndent
    | otherwise          -> Left      $ SpuriousDelimiter n c

  (Just Bird, Just Bird) -> continue  $ [stripBird' ws l]
  (Just _o  , Just Bird) -> continue  $ [l]
  (Just o   , Just c)
    | o `match` c        -> close     $ lineIfKeepAll
    | otherwise          -> Left      $ SpuriousDelimiter n c

  where
    q'                    = isDelimiter (ss `or` all) l
    continueWith r ls' l' = (l' <>) <$> unlit' ws (ss `or` inferred q') r ls'
    open' ls'             = continueWith q' ls'
    open                  = open' ls
    continue              = continueWith q ls
    close                 = continueWith Nothing ls
    lineIfKeepAll         = case ws of WsKeepAll    -> [""]; WsKeepIndent -> []
    lineIfKeepIndent      = case ws of WsKeepIndent -> [""]; WsKeepAll -> []

relit :: Style -> Delimiter -> String -> Either Error String
relit ss ts = fmap unlines . relit' ss ts Nothing . zip [1..] . lines

emitBird :: String -> String
emitBird l | stripStart l == "" = ">"
           | otherwise          = "> " <> l

emitOpen :: Delimiter -> Maybe String -> [String]
emitOpen  Bird l = fmap emitBird (maybeToList l)
emitOpen  del  l = emitDelimiter (setBegin Begin del) : maybeToList l

emitCode :: Delimiter -> String -> String
emitCode Bird l = emitBird l
emitCode _    l = l

emitClose :: Delimiter -> Maybe String -> [String]
emitClose  Bird l = maybeToList l
emitClose  del  l = emitDelimiter (setBegin End $ setLang' Nothing del) : maybeToList l

relit' :: Style -> Delimiter -> State -> [(Int, String)] -> Either Error [String]
relit' _ _   Nothing    [] = Right []
relit' _ ts (Just Bird) [] = Right (emitClose ts Nothing)
relit' _ _  (Just o)    [] = Left $ UnexpectedEnd o
relit' ss ts q ((n, l):ls) = case (q, q') of

  (Nothing  , Nothing)   -> continue

  (Nothing  , Just Bird) -> blockOpen $ Just (stripBird l)
  (Nothing  , Just (Asciidoc Begin _))
    | Just ls' <- asciidocFence ls
                         -> blockOpen' ls' Nothing
  (Nothing  , Just c)
    | isBegin c          -> blockOpen Nothing
    | otherwise          -> Left $ SpuriousDelimiter n c

  (Just Bird, Nothing)   -> blockClose $ Just l
  (Just _o  , Nothing)   -> blockContinue l

  (Just Bird, Just Bird) -> blockContinue $ stripBird l
  (Just _o  , Just Bird) -> continue
  (Just o   , Just c)
    | o `match` c        -> blockClose Nothing
    | otherwise          -> Left $ SpuriousDelimiter n c

  where
    q'                 = isDelimiter (ss `or` all) l
    continueWith r ls' = relit' (ss `or` inferred q') ts r ls'
    continue           = (l :)                <$> continueWith q ls
    blockOpen' ls' l'  = (emitOpen  ts l' <>) <$> continueWith q' ls'
    blockOpen      l'  = blockOpen' ls l'
    blockContinue  l'  = (emitCode  ts l' :)  <$> continueWith q ls
    blockClose     l'  = (emitClose ts l' <>) <$> continueWith Nothing ls

data Error
  = SpuriousDelimiter Int Delimiter
  | UnexpectedEnd     Delimiter
  deriving (Eq, Show)

showError :: Error -> String
showError (UnexpectedEnd       q) = "unexpected end of file: unmatched " <> emitDelimiter q
showError (SpuriousDelimiter n q) = "at line " <>  (show n) <> ": spurious "  <> emitDelimiter q

