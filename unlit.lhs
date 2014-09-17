> {-# LANGUAGE OverloadedStrings #-}
> module Main where
>
> import Data.Text.Lazy (Text)
> import qualified Data.Text.Lazy as T
> import qualified Data.Text.Lazy.IO as T



What are literate programs?
===========================

There are several styles of literate programming. Most commonly,
these are: LaTeX-style code tags, Bird tags and Markdown fenced code
blocks.

Each of these styles is characterised by its own set of delimiters:

> data Delim = BeginCode  | EndCode
>            | Bird
>            | TildeFence | BacktickFence
>            deriving (Eq)

> instance Show Delim where
>   show BeginCode     = "\\begin{code}"
>   show EndCode       = "\\end{code}"
>   show Bird          = ">"
>   show TildeFence    = "~~~"
>   show BacktickFence = "```"

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

> isBird :: Text -> Bool
> isBird l = (l == ">") || ("> " `T.isPrefixOf` l)

Due to this definition, whenever we strip a bird tag, we also remove
a the first space following it.

> stripBird :: Text -> Text
> stripBird l
>   | l == ">" = ""
>   | otherwise = T.drop 2 l


Lastly, Markdown supports two styles of fenced codeblocks: using
tildes or using backticks.

> tildeFence, backtickFence :: Text
> tildeFence    = "~~~"
> backtickFence = "```"
>
> isTildeFence, isBacktickFence :: Text -> Bool
> isTildeFence    l = tildeFence    `T.isPrefixOf` l
> isBacktickFence l = backtickFence `T.isPrefixOf` l

These two fences have support for adding metadata, in the form of a
CSS-style dictionary (`{#mycode .haskell .numberLines startFrom=100}`)
for long fences or a list of classes for short fences.[^fenced-code-attributes][^fenced-code-indention]


In general, we will also need a function that checks, for a given
line, whether it conforms to *any* of the styles.

> isDelim :: Text -> Maybe Delim
> isDelim l
>   | isBeginCode     l = Just BeginCode
>   | isEndCode       l = Just EndCode
>   | isBird          l = Just Bird
>   | isTildeFence    l = Just TildeFence
>   | isBacktickFence l = Just BacktickFence
>   | otherwise         = Nothing

And, for the styles which use opening and closing brackets, we will
need a function that checks if these pairs match.

> match :: Delim -> Delim -> Bool
> match BeginCode     EndCode       = True
> match TildeFence    TildeFence    = True
> match BacktickFence BacktickFence = True
> match _             _             = False

Note that Bird-tags are notably absent from the `match` function, as
they are a special case.



What do we want `unlit` to do?
==============================

The `unlit` program that we will implement below will do the following:
it will read a literate program from the standard input---allowing one
or more styles of code block---and emit only the code to the standard
output.

The options for source styles are as follows:

> data SourceStyle
>   = Infer
>   | Style { name :: String, allowed :: [Delim] }
>
> latex, bird, markdown :: SourceStyle
> latex    = Style "LaTeX"    [BeginCode, EndCode]
> bird     = Style "Bird"     [Bird]
> markdown = Style "Markdown" [Bird, TildeFence, BacktickFence]

Additionally, when the source style is set to `Infer`, the program
will guess the style based on the first delimiter it encounters,
always guessing the most permissive style---i.e. when it encounters a
Bird-tag it will assume that it is dealing with a Markdown-style
literate file and also allow fenced code blocks.

> infer :: Maybe Delim -> SourceStyle -> SourceStyle
> infer  Nothing         Infer = Infer
> infer (Just BeginCode) Infer = latex
> infer (Just _)         Infer = markdown
> infer  Nothing         ss    = ss
> infer (Just del)       ss    = check del ss

> check :: Delim -> SourceStyle -> SourceStyle
> check del ss
>   | del `elem` allowed ss = ss
>   | otherwise = error ("delimiter " ++ show del ++ " disallowed in " ++ name ss)

Thus, the `unlit` function will have two parameters: its source style
and the text to convert.

> unlit :: SourceStyle -> [Text] -> [Text]
> unlit ss = unlit' ss Nothing


However, the helper function `unlit'` is best thought of as a finite
state automaton, where the states are used to remember the what kind
of code block (if any) the automaton currently is in.

> type State = Maybe Delim

> unlit' :: SourceStyle -> State -> [Text] -> [Text]
> unlit' _ _ [] = []
> unlit' ss q (l:ls) = case (q, q') of
>
>   (Nothing   , Nothing)      -> continue
>   (Nothing   , Just Bird)    -> stripBird l : openBlock
>   (Just Bird , Just Bird)    -> stripBird l : continue
>   (Just Bird , Nothing)      ->               closeBlock
>   (Nothing   , Just EndCode) -> spurious EndCode
>   (Nothing   , Just o)       -> T.empty     : openBlock
>   (Just o    , Nothing)      -> l           : continue
>   (Just o    , Just c)       -> if match o c then T.empty : closeBlock else spurious c
>
>   where
>     q'             = isDelim l
>     continueWith q = unlit' (infer q' ss) q ls
>     openBlock      = continueWith q'
>     continue       = continueWith q
>     closeBlock     = continueWith Nothing
>     spurious q     = error ("spurious " ++ show q)

[^fenced-code-attributes]: http://johnmacfarlane.net/pandoc/demo/example9/pandocs-markdown.html#extension-fenced_code_attributes
[^fenced-code-indention]: At the moment we don't support fenced code block indentation.

> main :: IO ()
> main = T.getContents >>= sequence_ . map T.putStrLn . unlit Infer . T.lines
