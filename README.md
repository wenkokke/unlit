[![Build Status](https://travis-ci.org/pepijnkokke/unlit.png?branch=master)](https://travis-ci.org/pepijnkokke/unlit)
```haskell
{-# LANGUAGE GADTs, OverloadedStrings #-}
module Unlit.Text
       (unlit, relit
       ,Style, all, latex, bird, haskell, markdown, tildefence, backtickfence
       ,Lang, forLang) where
```
```haskell
import Prelude hiding (all, or, drop, dropWhile, takeWhile, length, lines, unlines, getContents, putStrLn)
import Control.Monad (msum)
import Data.Char (isSpace)
import Data.Maybe (maybe, maybeToList, listToMaybe, fromMaybe)
import Data.Monoid (mempty,(<>))
import Data.Text (Text)
import Data.Text (drop, dropWhile, takeWhile, length, lines, unlines, pack, unpack, isPrefixOf, isInfixOf)
import Data.Text.IO (getContents, putStrLn)
```


What are literate programs?
===========================

There are several styles of literate programming. Most commonly,
these are LaTeX-style code tags, Bird tags and Markdown fenced code
blocks.

```haskell
data Delim where
  LaTeX         :: BeginEnd -> Delim
  Bird          :: Delim
  TildeFence    :: Maybe Lang -> Delim
  BacktickFence :: Maybe Lang -> Delim
  deriving (Eq)
```
Some of these code blocks need to  carry around additional information.
For instance, LaTex code blocks use distinct opening and closing tags.

```haskell
data BeginEnd where
  Begin :: BeginEnd
  End   :: BeginEnd
  deriving (Eq)
```
On the other hand, Markdown-style fenced code blocks can be annotated
with all sorts of information. Most prominently, their programming
language.

```haskell
type Lang = Text
```
In order to properly show these code blocks, we will define the
following instance.

```haskell
instance Show Delim where
  show (LaTeX Begin)     = "\\begin{code}"
  show (LaTeX End)       = "\\end{code}"
  show  Bird             = ">"
  show (TildeFence l)    = "~~~" ++ (maybe "" unpack l)
  show (BacktickFence l) = "```" ++ (maybe "" unpack l)
```
Furthermore, we need a set of functions which is able to recognise
these code blocks.

```haskell
type Recogniser = Text -> Maybe Delim
```
```haskell
infix 1 ?:

(?:) :: Bool -> a -> Maybe a
True  ?: x = Just x
False ?: _ = Nothing
```
For instance, in LaTeX-style, a codeblock is delimited by
`\begin{code}` and `\end{code}` tags, which must appear at the first
position (since we do not support indented code blocks).

```haskell
isLaTeX :: Recogniser
isLaTeX l
  | "\\begin{code}" `isPrefixOf` lstrip l = return $ LaTeX Begin
  | "\\end{code}"   `isPrefixOf` lstrip l = return $ LaTeX End
  | otherwise = Nothing
```
However, there is the optional feature of indented code blocks, for
which we'll need to remember how many whitespaces to remove.

```haskell
lstrip :: Text -> Text
lstrip = dropWhile isSpace
```
In Bird-style, every line in a codeblock must start with a Bird tag.
A tagged line is defined as *either* a line containing solely the
symbol '>', or a line starting with the symbol '>' followed by at
least one space.

```haskell
isBird :: Recogniser
isBird l = (l == ">") || ("> " `isPrefixOf` l) ?: Bird
```
Due to this definition, whenever we strip a bird tag, we also remove
a the first space following it.

```haskell
stripBird :: Text -> Text
stripBird l = drop 2 l
```
Lastly, Markdown supports two styles of fenced codeblocks: using
tildes or using backticks. These fenced codeblocks have as a
peculiarity that they can be defined to only match on fences for a
certain language.

Below we only check if the given language occurs *anywhere* in the
string; we don't bother parsing the entire line to see if it's
well-formed Markdown.

```haskell
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
```
```haskell
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
```
In general, we will also need a function that checks, for a given
line, whether it conforms to *any* of a set of given styles.

```haskell
isDelim :: [Delim] -> Recogniser
isDelim ds l = msum (map go ds)
  where
    go :: Delim -> Maybe Delim
    go (LaTeX _)            = isLaTeX l
    go  Bird                = isBird l
    go (TildeFence lang)    = isTildeFence lang l
    go (BacktickFence lang) = isBacktickFence lang l
```
And, for the styles which use opening and closing brackets, we will
need a function that checks if these pairs match.

```haskell
match :: Delim -> Delim -> Bool
match (LaTeX Begin)     (LaTeX End)             = True
match (TildeFence _)    (TildeFence Nothing)    = True
match (BacktickFence _) (BacktickFence Nothing) = True
match  _                 _                      = False
```
Note that Bird-tags are notably absent from the `match` function, as
they are a special case.




What do we want `unlit` to do?
==============================

The `unlit` program that we will implement below will do the following:
it will read a literate program from the standard input---allowing one
or more styles of code block---and emit only the code to the standard
output.

The options for source styles are as follows:

```haskell
type Style = [Delim]

bird             = [Bird]
latex            = [LaTeX Begin, LaTeX End]
haskell          = latex ++ bird
tildefence       = [TildeFence Nothing]
backtickfence    = [BacktickFence Nothing]
markdown         = bird ++ tildefence ++ backtickfence
all              = latex ++ markdown
infer            = []
```
```haskell
forLang :: Lang -> Style -> Style
forLang l = map (setLang (Just l))
```
```haskell
setLang :: Maybe Lang -> Delim -> Delim
setLang l (TildeFence _)    = TildeFence l
setLang l (BacktickFence _) = BacktickFence l
setLang _  d                = d
```
Additionally, when the source style is empty, the program will
attempt to guess the style based on the first delimiter it
encounters. It will try to be permissive in this, and therefore, if
it encounters a Bird-tag, will infer general Markdown-style.

```haskell
doInfer :: Maybe Delim -> [Delim]
doInfer  Nothing         = []
doInfer (Just (LaTeX _)) = latex
doInfer (Just _)         = markdown
```
We would like to combine the inferred style with current styles as
one would combine maybe values using the alternative operator
`(<|>)`. Therefore, we will define our own version of this operator.

```haskell
or :: [a] -> [a] -> [a]
xs `or` [] = xs
[] `or` ys = ys
xs `or` ys = xs
```
Thus, the `unlit` function will have two parameters: its source style
and the text to convert.

```haskell
unlit :: [Delim] -> Text -> Text
unlit ss = unlines . unlit' ss Nothing 0 . zip [1..] . lines
```
However, the helper function `unlit'` is best thought of as a finite
state automaton, where the states are used to remember the what kind
of code block (if any) the automaton currently is in.

```haskell
type State = Maybe Delim
```
Additionally, for the optional feature of indented code blocks, we will
have to be able to count the number of whitespaces to remove. This
will be passed as the second additional argument.

```haskell
countSpaces :: Text -> Int
countSpaces = length . takeWhile isSpace
```
With this, the signature of `unlit'` becomes:

```haskell
unlit' :: [Delim] -> State -> Int -> [(Int, Text)] -> [Text]
unlit' _ _ _ [] = []
unlit' ss q ws ((n, l):ls) = case (q, q') of


  (Nothing  , Nothing)          -> continue
  (Nothing  , Just Bird)        -> blockOpen     $ Just (stripBird l)
  (Just Bird, Just Bird)        -> blockContinue $ stripBird l
  (Just Bird, Nothing)          -> blockClose
  (Nothing  , Just (LaTeX End)) -> spurious (LaTeX End)
  (Nothing  , Just o)           -> blockOpen     $ Nothing
  (Just o   , Nothing)          -> blockContinue $ l
  (Just o   , Just Bird)        -> l : continue
  (Just o   , Just c)           -> if o `match` c then blockClose else spurious c

  where
    q'                = isDelim (ss `or` all) l
    continueWith q ws = unlit' (ss `or` doInfer q') q ws ls
    continue          = continueWith q ws
    blockOpen       l = maybeToList l ++ continueWith q' ws
    blockContinue   l = l : continue
    blockClose        = mempty : continueWith Nothing ws
    spurious        q = error ("at line " ++ show n ++ ": spurious " ++ show q)
```



What do we want `relit` to do?
==============================

Sadly, no, `relit` won't be able to take source code and
automatically convert it to literate code. I'm not quite up to the
challenge of automatically generating meaningful documentation from
arbitrary code... I wish I was.

What `relit` will do is read a literate file using one style of
delimiters and emit the same file using an other style of delimiters.

```haskell
relit :: Style -> Style -> Text -> Text
relit ss ts = unlines . relit' ss (head ts) Nothing . zip [1..] . lines
```
Again, we will interpret the helper function `relit'` as an
automaton, which remembers the current state. However, we now also
need a function which can emit code blocks in a certain style. For
this purpose we will define a triple of functions.

```haskell
emitBird :: Text -> Text
emitBird l = "> " <> l

emitOpen :: Delim -> Maybe Text -> [Text]
emitOpen  Bird       l = mempty : map emitBird (maybeToList l)
emitOpen (LaTeX End) l = emitOpen (LaTeX Begin) l
emitOpen  del        l = pack (show del) : maybeToList l

emitCode :: Delim -> Text -> Text
emitCode Bird l = emitBird l
emitCode _    l = l

emitClose :: Delim -> Text
emitClose  Bird         = mempty
emitClose (LaTeX Begin) = emitClose (LaTeX End)
emitClose  del          = pack (show (setLang Nothing del))
```
Using these simple functions we can easily define the `relit'`
function.

```haskell
relit' :: Style -> Delim -> State -> [(Int, Text)] -> [Text]
relit' _ _   Nothing    [] = []
relit' _ ts (Just Bird) [] = emitClose ts : []
relit' _ _  (Just o)    [] = error ("unexpected EOF; unmatched " ++ show o)
relit' ss ts q ((n, l):ls) = case (q, q') of

  (Nothing  , Nothing)          -> l : continue
  (Nothing  , Just Bird)        -> blockOpen     $ Just (stripBird l)
  (Just Bird, Just Bird)        -> blockContinue $ stripBird l
  (Just Bird, Nothing)          -> blockClose
  (Nothing  , Just (LaTeX End)) -> spurious (LaTeX End)
  (Nothing  , Just o)           -> blockOpen     $ Nothing
  (Just o   , Nothing)          -> blockContinue $ l
  (Just o   , Just Bird)        -> l : continue
  (Just o   , Just c)           -> if o `match` c then blockClose else spurious c

  where
    q'              = isDelim (ss `or` all) l
    continueWith  q = relit' (ss `or` doInfer q') ts q ls
    continue        = continueWith q
    blockOpen     l = emitOpen  ts l ++ continueWith q'
    blockContinue l = emitCode  ts l : continue
    blockClose      = emitClose ts   : continueWith Nothing
    spurious      q = error ("at line " ++ show n ++ ": spurious " ++ show q)
```

