> {-# LANGUAGE OverloadedStrings #-}
> module Unlit.Text (unlit, relit,
>                    Style, all, latex, bird, haskell, markdown,
>                    tildefence, backtickfence,
>                    Language, forLanguage) where
>
>
> import Prelude hiding (or,all)
> import Control.Applicative ((<|>))
> import Control.Monad (msum)
> import Data.Maybe (maybe,maybeToList,listToMaybe,fromMaybe)
> import Data.Monoid (mempty,(<>))
> import Data.Text.Lazy (Text)
> import qualified Data.Text.Lazy as T
> import qualified Data.Text.Lazy.IO as T (getContents,putStrLn)

> or :: [a] -> [a] -> [a]
> xs `or` [] = xs
> [] `or` ys = ys
> xs `or` ys = xs

What are literate programs?
===========================

There are several styles of literate programming. Most commonly,
these are: LaTeX-style code tags, Bird tags and Markdown fenced code
blocks.

Each of these styles is characterised by its own set of delimiters:

> data Delim
>   = BeginCode
>   | EndCode
>   | BirdTag
>   | TildeFence    (Maybe Language)
>   | BacktickFence (Maybe Language)
>   deriving (Eq)

> instance Show Delim where
>   show  BeginCode        = "\\begin{code}"
>   show  EndCode          = "\\end{code}"
>   show  BirdTag          = ">"
>   show (TildeFence l)    = "~~~" ++ (maybe "" T.unpack l)
>   show (BacktickFence l) = "```" ++ (maybe "" T.unpack l)

In LaTeX-style, a codeblock is delimited by `\begin{code}` and
`\end{code}` tags.

> beginCode, endCode :: Text
> beginCode = "\\begin{code}"
> endCode   = "\\end{code}"
>
> isBeginCode, isEndCode :: Text -> Bool
> isBeginCode l = beginCode `T.isPrefixOf` l
> isEndCode   l = endCode   `T.isPrefixOf` l


In Bird-style, every line in a codeblock must start with a Bird tag.
A tagged line is defined as *either* a line containing solely the
symbol '>', or a line starting with the symbol '>' followed by at
least one space.

> isBirdTag :: Text -> Bool
> isBirdTag l = (l == ">") || ("> " `T.isPrefixOf` l)

Due to this definition, whenever we strip a bird tag, we also remove
a the first space following it.

> stripBirdTag :: Text -> Text
> stripBirdTag l
>   | l == ">" = ""
>   | otherwise = T.drop 2 l


Lastly, Markdown supports two styles of fenced codeblocks: using
tildes or using backticks. These fenced codeblocks have as a
peculiarity that they can be defined to only match on fences for a
certain language.

> type Language = Text

> tildeFence, backtickFence :: Text
> tildeFence    = "~~~"
> backtickFence = "```"
>
> isTildeFence, isBacktickFence :: Maybe Language -> Text -> Bool
> isTildeFence    lang l =
>   tildeFence `T.isPrefixOf` l && (maybe True (`T.isInfixOf` l) lang)
> isBacktickFence lang l =
>   backtickFence `T.isPrefixOf` l && (maybe True (`T.isInfixOf` l) lang)

These two fences have support for adding metadata, in the form of a
CSS-style dictionary (`{#mycode .haskell .numberLines startFrom=100}`)
for long fences or a list of classes for short fences.[^fenced-code-attributes]


In general, we will also need a function that checks, for a given
line, whether it conforms to *any* of the styles.

> (?:) :: Bool -> a -> Maybe a
> True  ?: x = Just x
> False ?: _ = Nothing

> isDelim :: [Delim] -> Text -> Maybe Delim
> isDelim ds l = msum (map (go l) ds)
>   where
>     go :: Text -> Delim -> Maybe Delim
>     go l  BeginCode           = isBeginCode l          ?: BeginCode
>     go l  EndCode             = isEndCode l            ?: EndCode
>     go l  BirdTag             = isBirdTag l            ?: BirdTag
>     go l (TildeFence    lang) = isTildeFence lang l    ?: TildeFence lang
>     go l (BacktickFence lang) = isBacktickFence lang l ?: BacktickFence lang

And, for the styles which use opening and closing brackets, we will
need a function that checks if these pairs match.

> match :: Delim -> Delim -> Bool
> match  BeginCode         EndCode                = True
> match (TildeFence _)    (TildeFence Nothing)    = True
> match (BacktickFence _) (BacktickFence Nothing) = True
> match _                  _                      = False

Note that Bird-tags are notably absent from the `match` function, as
they are a special case.



What do we want `unlit` to do?
==============================

The `unlit` program that we will implement below will do the following:
it will read a literate program from the standard input---allowing one
or more styles of code block---and emit only the code to the standard
output.

The options for source styles are as follows:

> type Style = [Delim]
>
> latex, bird, markdown :: Style
> bird             = [BirdTag]
> latex            = [BeginCode, EndCode]
> haskell          = latex ++ bird
> tildefence       = [TildeFence Nothing]
> backtickfence    = [BacktickFence Nothing]
> markdown         = bird ++ tildefence ++ backtickfence
> all              = latex ++ markdown

> forLanguage :: Language -> Style -> Style
> forLanguage l = map (setLanguage (Just l))

> setLanguage :: Maybe Language -> Delim -> Delim
> setLanguage l (TildeFence _)    = TildeFence l
> setLanguage l (BacktickFence _) = BacktickFence l
> setLanguage _  d                = d

Additionally, when the source style is set to `Nothing`, the program
will guess the style based on the first delimiter it encounters,
always guessing the most permissive style---i.e. when it encounters a
Bird-tag it will assume that it is dealing with a Markdown-style
literate file and also allow fenced code blocks.

> infer :: Maybe Delim -> Style
> infer  Nothing         = []
> infer (Just BeginCode) = latex
> infer (Just _)         = markdown

Thus, the `unlit` function will have two parameters: its source style
and the text to convert.

> unlit :: Style -> Text -> Text
> unlit ss = T.unlines . unlit' ss Nothing . zip [1..] . T.lines

However, the helper function `unlit'` is best thought of as a finite
state automaton, where the states are used to remember the what kind
of code block (if any) the automaton currently is in.

> type State = Maybe Delim

> unlit' :: Style -> State -> [(Int, Text)] -> [Text]
> unlit' _ _ [] = []
> unlit' ss q ((n, l):ls) = case (q, q') of
>
>   (Nothing      , Nothing)      -> continue
>   (Nothing      , Just BirdTag) -> blockOpen     $ Just (stripBirdTag l)
>   (Just BirdTag , Just BirdTag) -> blockContinue $ stripBirdTag l
>   (Just BirdTag , Nothing)      -> blockClose
>   (Nothing      , Just EndCode) -> spurious EndCode
>   (Nothing      , Just o)       -> blockOpen     $ Nothing
>   (Just o       , Nothing)      -> blockContinue $ l
>   (Just o       , Just c)       -> if o `match` c then blockClose else spurious c
>
>   where
>     q'              = isDelim (ss `or` all) l
>     continueWith  q = unlit' (ss `or` infer q') q ls
>     continue        = continueWith q
>     blockOpen     l = maybeToList l ++ continueWith q'
>     blockContinue l = l : continue
>     blockClose      = mempty : continueWith Nothing
>     spurious      q = error ("at line " ++ show n ++ ": spurious " ++ show q)



What do we want `relit` to do?
==============================

Sadly, no, `relit` won't be able to take source code and
automatically convert it to literate code. I'm not quite up to the
challenge of automatically generating meaningful documentation from
arbitrary code... I wish I was.

What `relit` will do is read a literate file using one style of
delimiters and emit the same file using an other style of delimiters.

> relit :: Style -> Style -> Text -> Text
> relit ss ts = T.unlines . relit' ss (head ts) Nothing . zip [1..] . T.lines

Again, we will interpret the helper function `relit'` as an
automaton, which remembers the current state. However, we now also
need a function which can emit code blocks in a certain style. For
this purpose we will define a triple of functions.

> emitBirdTag :: Text -> Text
> emitBirdTag l = "> " <> l
>
> emitOpen :: Delim -> Maybe Text -> [Text]
> emitOpen BirdTag l = mempty            : map emitBirdTag (maybeToList l)
> emitOpen EndCode l = beginCode         : maybeToList l
> emitOpen del     l = T.pack (show del) : maybeToList l
>
> emitCode :: Delim -> Text -> Text
> emitCode BirdTag l = emitBirdTag l
> emitCode _       l = l
>
> emitClose :: Delim -> Text
> emitClose BirdTag   = mempty
> emitClose BeginCode = endCode
> emitClose del       = T.pack (show (setLanguage Nothing del))

Using these simple functions we can easily define the `relit'`
function.

> relit' :: Style -> Delim -> State -> [(Int, Text)] -> [Text]
> relit' _ _ _ [] = []
> relit' ss ts q ((n, l):ls) = case (q, q') of
>
>   (Nothing      , Nothing)      -> l : continue
>   (Nothing      , Just BirdTag) -> blockOpen     $ Just (stripBirdTag l)
>   (Just BirdTag , Just BirdTag) -> blockContinue $ stripBirdTag l
>   (Just BirdTag , Nothing)      -> blockClose
>   (Nothing      , Just EndCode) -> spurious EndCode
>   (Nothing      , Just o)       -> blockOpen     $ Nothing
>   (Just o       , Nothing)      -> blockContinue $ l
>   (Just o       , Just c)       -> if o `match` c then blockClose else spurious c
>
>   where
>     q'              = isDelim (ss `or` all) l
>     continueWith  q = relit' (ss `or` infer q') ts q ls
>     continue        = continueWith q
>     blockOpen     l = emitOpen  ts l ++ continueWith q'
>     blockContinue l = emitCode  ts l : continue
>     blockClose      = emitClose ts   : continueWith Nothing
>     spurious      q = error ("at line " ++ show n ++ ": spurious " ++ show q)

[^fenced-code-attributes]: http://johnmacfarlane.net/pandoc/demo/example9/pandocs-markdown.html#extension-fenced_code_attributes
