[![Build Status](https://travis-ci.org/wenkokke/unlit.png?branch=master)](https://travis-ci.org/wenkokke/unlit)
``` haskell
{-# LANGUAGE OverloadedStrings #-}
module Unlit.Text (
  unlit, relit
  , Style, parseStyle
  , WhitespaceMode(..), parseWhitespaceMode
  , all, infer, latex, bird, jekyll,  haskell, markdown, tildefence, backtickfence
  , Lang, setLang
  , Error(..), showError
) where
```
``` haskell
import Data.Functor ((<$>))
import Data.Foldable (asum)
import Data.Bool (bool)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Monoid ((<>))
import Prelude hiding (all, or, String, unlines, lines, drop)
import Data.Text (Text, stripStart, stripEnd, isPrefixOf, isSuffixOf, isInfixOf, unlines, lines, pack, drop, toLower)
```
What are literate programs?
===========================

There are several styles of literate programming. Most commonly,
these are LaTeX-style code tags, Bird tags and Markdown fenced code
blocks.

``` haskell
data Delimiter
  = LaTeX    BeginEnd
  | OrgMode  BeginEnd Lang
  | Bird
  | Jekyll   BeginEnd Lang
  | Markdown Fence Lang
  | Asciidoc BeginEnd Lang
  deriving (Eq, Show)
```
Some of these code blocks need to carry around additional information.
For instance, LaTex code blocks use distinct opening and closing tags.

``` haskell
data BeginEnd
  = Begin
  | End
  deriving (Eq, Show)
```
``` haskell
isBegin :: Delimiter -> Bool
isBegin (LaTeX    Begin  ) = True
isBegin (OrgMode  Begin _) = True
isBegin (Jekyll   Begin _) = True
isBegin (Asciidoc Begin _) = True
isBegin (Markdown _ _)     = True
isBegin  _                 = False
```
``` haskell
setBegin :: BeginEnd -> Delimiter -> Delimiter
setBegin beginEnd (LaTeX    _  )    = LaTeX    beginEnd
setBegin beginEnd (OrgMode  _ lang) = OrgMode  beginEnd lang
setBegin beginEnd (Jekyll   _ lang) = Jekyll   beginEnd lang
setBegin beginEnd (Asciidoc _ lang) = Asciidoc beginEnd lang
setBegin _         del              = del
```
On the other hand, Markdown-style fences occur in two different variants.

``` haskell
data Fence
  = Tilde
  | Backtick
  deriving (Eq, Show)
```
Furthermore they may be annotated with all sorts of information. Most prominently,
their programming language.

``` haskell
type Lang = Maybe Text
```
``` haskell
containsLang :: Text -> Lang -> Bool
containsLang _ Nothing     = True
containsLang l (Just lang) = toLower lang `isInfixOf` toLower l
```
In order to emit these code blocks, we will define the
following function.

``` haskell
emitDelimiter :: Delimiter -> Text
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
```
``` haskell
infixr 5 <+>
(<+>) :: Text -> Text -> Text
"" <+> y  = y
x  <+> "" = x
x  <+> y  = x <> " " <> y
```
Furthermore, we need a set of functions which is able to recognise
these code blocks.

``` haskell
type Recogniser = Text -> Maybe Delimiter
```
For instance, in LaTeX-style, a codeblock is delimited by
`\begin{code}` and `\end{code}` tags, which must appear at the first
position (since we do not support indented code blocks).

``` haskell
isLaTeX :: Recogniser
isLaTeX l
  | "\\begin{code}" `isPrefixOf` stripStart l = Just $ LaTeX Begin
  | "\\end{code}"   `isPrefixOf` stripStart l = Just $ LaTeX End
  | otherwise = Nothing
```
``` haskell
isOrgMode :: Lang -> Recogniser
isOrgMode lang l
  | "#+BEGIN_SRC" `isPrefixOf` stripStart l
    && l `containsLang` lang                = Just $ OrgMode Begin lang
  | "#+END_SRC"   `isPrefixOf` stripStart l = Just $ OrgMode End Nothing
  | otherwise = Nothing
```
In Bird-style, every line in a codeblock must start with a Bird tag.
A tagged line is defined as *either* a line containing solely the
symbol '>', or a line starting with the symbol '>' followed by at
least one space.

``` haskell
isBird :: Recogniser
isBird l = bool Nothing (Just Bird) (l == ">" || "> " `isPrefixOf` l)
```
Due to this definition, whenever we strip a bird tag, in normal
whitespace modes we also remove the first space following it.

``` haskell
stripBird :: Text -> Text
stripBird = stripBird' WsKeepIndent
```
``` haskell
stripBird' :: WhitespaceMode -> Text -> Text
stripBird' WsKeepAll    l = " " <> drop 1 l
stripBird' WsKeepIndent l = drop 2 l
```
Then we have Jekyll Liquid code blocks.

``` haskell
isJekyll :: Lang -> Recogniser
isJekyll lang l
  | "{% highlight" `isPrefixOf` stripStart l
    && l `containsLang` lang
    && "%}" `isSuffixOf` stripEnd l     = Just $ Jekyll Begin lang
  | "{% endhighlight %}" `isPrefixOf` l = Just $ Jekyll End   lang
  | otherwise                           = Nothing
```
Markdown fenced codeblocks have as a peculiarity that they
can be defined to only match on fences for a certain language.

Below we only check if the given language occurs *anywhere* in the
string; we don't bother parsing the entire line to see if it's
well-formed Markdown.

``` haskell
isMarkdown :: Fence -> Text -> Lang -> Recogniser
isMarkdown fence fenceStr lang l
  | fenceStr `isPrefixOf` stripStart l =
    Just $ Markdown fence $ bool Nothing lang (l `containsLang` lang)
  | otherwise = Nothing
```
The Asciidoc fence in the beginning takes two lines, `[source,lang]` and `----`.
Here we just check for the source line. The second line will be consumed by asciidocBlock.

``` haskell
isAsciidoc :: Lang -> Recogniser
isAsciidoc lang l
  | "[source" `isPrefixOf` l
    && l `containsLang` lang
    && "]" `isSuffixOf` stripEnd l = Just $ Asciidoc Begin lang
  | "----" `isPrefixOf` l          = Just $ Asciidoc End   lang
  | otherwise                      = Nothing
```
``` haskell
asciidocFence :: [(Int,Text)] -> Maybe [(Int,Text)]
asciidocFence ls | ((_,"----"):ls') <- ls = Just ls'
                 | otherwise              = Nothing
```
In general, we will also need a function that checks, for a given
line, whether it conforms to *any* of a set of given styles.

``` haskell
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
```
And, for the styles which use opening and closing brackets, we will
need a function that checks if these pairs match.

``` haskell
match :: Delimiter -> Delimiter -> Bool
match (LaTeX Begin)      (LaTeX End)          = True
match (Jekyll Begin _)   (Jekyll End _)       = True
match (OrgMode Begin _)  (OrgMode End _)      = True
match (Asciidoc Begin _) (Asciidoc End _)     = True
match (Markdown f _)     (Markdown g Nothing) = f == g
match  _                  _                   = False
```
Note that Bird-tags are notably absent from the `match` function, as
they are a special case.

What do we want `unlit` to do?
==============================

The `unlit` program that we will implement below will do the following:
it will read a literate program from the standard input—allowing one
or more styles of code block—and emit only the code to the standard
output.

The options for source styles are as follows:

``` haskell
type Style = [Delimiter]
```
``` haskell
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
```
``` haskell
parseStyle :: Text -> Maybe Style
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
```
It is possible to set the language of the source styles using the following function.

``` haskell
setLang :: Lang -> Style -> Style
setLang = fmap . setLang'
```
``` haskell
setLang' :: Lang -> Delimiter -> Delimiter
setLang' lang (Markdown fence _)   = Markdown fence lang
setLang' lang (OrgMode beginEnd _) = OrgMode beginEnd lang
setLang' lang (Jekyll beginEnd _)  = Jekyll beginEnd lang
setLang' _     d                   = d
```
Additionally, when the source style is empty, the program will
attempt to guess the style based on the first delimiter it
encounters. It will try to be permissive in this, and therefore, if
it encounters a Bird-tag, will infer general Markdown-style.

``` haskell
inferred :: Maybe Delimiter -> Style
inferred  Nothing              = []
inferred (Just (LaTeX _))      = latex
inferred (Just (Jekyll _ _))   = jekyll
inferred (Just (OrgMode _ _))  = orgmode
inferred (Just (Asciidoc _ _)) = asciidoc
inferred (Just _)              = markdown
```
Lastly, we would like `unlit` to be able to operate in several
different whitespace modes. For now, these are:

``` haskell
data WhitespaceMode
  = WsKeepIndent -- ^ keeps only indentations
  | WsKeepAll    -- ^ keeps all lines and whitespace
```
``` haskell
parseWhitespaceMode :: Text -> Maybe WhitespaceMode
parseWhitespaceMode s = case toLower s of
  "all"    -> Just WsKeepAll
  "indent" -> Just WsKeepIndent
  _        -> Nothing
```
We would like to combine the inferred style with current styles as
one would combine maybe values using the alternative operator
`(<|>)`. Therefore, we will define our own version of this operator.

``` haskell
or :: [a] -> [a] -> [a]
xs `or` [] = xs
[] `or` ys = ys
xs `or` _  = xs
```
Thus, the `unlit` function will have two parameters: its source style
and the text to convert.

``` haskell
unlit :: WhitespaceMode -> Style -> Text -> Either Error Text
unlit ws ss = fmap unlines . unlit' ws ss Nothing . zip [1..] . lines
```
However, the helper function `unlit'` is best thought of as a finite
state automaton, where the states are used to remember the what kind
of code block (if any) the automaton currently is in.

``` haskell
type State = Maybe Delimiter
```
With this, the signature of `unlit'` becomes:

``` haskell
unlit' :: WhitespaceMode -> Style -> State -> [(Int, Text)] -> Either Error [Text]
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
```
What do we want `relit` to do?
==============================

Sadly, no, `relit` won't be able to take source code and
automatically convert it to literate code. I'm not quite up to the
challenge of automatically generating meaningful documentation from
arbitrary code... I wish I was.

What `relit` will do is read a literate file using one style of
delimiters and emit the same file using an other style of delimiters.

``` haskell
relit :: Style -> Delimiter -> Text -> Either Error Text
relit ss ts = fmap unlines . relit' ss ts Nothing . zip [1..] . lines
```
Again, we will interpret the helper function `relit'` as an
automaton, which remembers the current state. However, we now also
need a function which can emit code blocks in a certain style. For
this purpose we will define a few functions.

TODO: Currently, if a delimiter is indented, running `relit` will remove this
      indentation. This is obviously an error, however changing it would require
      adding indentation information to all delimiters.

``` haskell
emitBird :: Text -> Text
emitBird l | stripStart l == "" = ">"
           | otherwise          = "> " <> l
```
``` haskell
emitOpen :: Delimiter -> Maybe Text -> [Text]
emitOpen  Bird l = fmap emitBird (maybeToList l)
emitOpen  del  l = emitDelimiter (setBegin Begin del) : maybeToList l
```
``` haskell
emitCode :: Delimiter -> Text -> Text
emitCode Bird l = emitBird l
emitCode _    l = l
```
``` haskell
emitClose :: Delimiter -> Maybe Text -> [Text]
emitClose  Bird l = maybeToList l
emitClose  del  l = emitDelimiter (setBegin End $ setLang' Nothing del) : maybeToList l
```
Using these simple functions we can easily define the `relit'`
function.

``` haskell
relit' :: Style -> Delimiter -> State -> [(Int, Text)] -> Either Error [Text]
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
```
Error handling
==============

In case of an error both `unlit` and `relit` return a value of the datatype `Error`.

``` haskell
data Error
  = SpuriousDelimiter Int Delimiter
  | UnexpectedEnd     Delimiter
  deriving (Eq, Show)
```
We can get a text representation of the error using `showError`.

``` haskell
showError :: Error -> Text
showError (UnexpectedEnd       q) = "unexpected end of file: unmatched " <> emitDelimiter q
showError (SpuriousDelimiter n q) = "at line " <> pack (show n) <> ": spurious "  <> emitDelimiter q
```

