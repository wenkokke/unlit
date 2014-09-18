> {-# LANGUAGE OverloadedStrings #-}
> import Prelude hiding (all)
> import Control.Applicative ((<|>))
> import Data.Char (toLower)
> import Data.Maybe (maybe,maybeToList,listToMaybe,fromMaybe)
> import Data.Text.Lazy (Text)
> import qualified Data.Text.Lazy as T
> import qualified Data.Text.Lazy.IO as T
> import System.Environment (getArgs,getProgName)
> import System.Exit (exitWith,ExitCode(..))
> import System.IO (hPutStrLn,stderr)
> import System.Console.GetOpt (OptDescr(..),ArgDescr(..),ArgOrder(..),usageInfo,getOpt)



What are literate programs?
===========================

There are several styles of literate programming. Most commonly,
these are: LaTeX-style code tags, Bird tags and Markdown fenced code
blocks.

Each of these styles is characterised by its own set of delimiters:

> data Delim = BeginCode  | EndCode
>            | BirdTag
>            | TildeFence | BacktickFence
>            deriving (Eq)

> instance Show Delim where
>   show BeginCode     = "\\begin{code}"
>   show EndCode       = "\\end{code}"
>   show BirdTag       = ">"
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

> isBirdTag :: Text -> Bool
> isBirdTag l = (l == ">") || ("> " `T.isPrefixOf` l)

Due to this definition, whenever we strip a bird tag, we also remove
a the first space following it.

> stripBirdTag :: Text -> Text
> stripBirdTag l
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
for long fences or a list of classes for short fences.[^fenced-code-attributes]


In general, we will also need a function that checks, for a given
line, whether it conforms to *any* of the styles.

> isDelim :: [Delim] -> Text -> Maybe Delim
> isDelim ds l =
>   listToMaybe . map fst . filter (\(d,p) -> d `elem` ds && p l) $ detectors
>   where
>     detectors :: [(Delim, Text -> Bool)]
>     detectors =
>       [ (BeginCode     , isBeginCode)
>       , (EndCode       , isEndCode)
>       , (BirdTag       , isBirdTag)
>       , (TildeFence    , isTildeFence)
>       , (BacktickFence , isBacktickFence)
>       ]

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

> data Name  = LaTeX | Bird | Markdown | All deriving (Show)
> data Style = Style { name :: Name, allowed :: [Delim] }
>
> latex, bird, markdown :: Style
> all      = Style All      [BeginCode, EndCode, BirdTag, TildeFence, BacktickFence]
> latex    = Style LaTeX    [BeginCode, EndCode]
> bird     = Style Bird     [BirdTag]
> markdown = Style Markdown [BirdTag, TildeFence, BacktickFence]

Additionally, when the source style is set to `Nothing`, the program
will guess the style based on the first delimiter it encounters,
always guessing the most permissive style---i.e. when it encounters a
Bird-tag it will assume that it is dealing with a Markdown-style
literate file and also allow fenced code blocks.

> infer :: Maybe Delim -> Maybe Style
> infer  Nothing         = Nothing
> infer (Just BeginCode) = Just latex
> infer (Just _)         = Just markdown

Thus, the `unlit` function will have two parameters: its source style
and the text to convert.

> unlit :: Maybe Style -> [(Int, Text)] -> [Text]
> unlit ss = unlit' ss Nothing

However, the helper function `unlit'` is best thought of as a finite
state automaton, where the states are used to remember the what kind
of code block (if any) the automaton currently is in.

> type State = Maybe Delim

> unlit' :: Maybe Style -> State -> [(Int, Text)] -> [Text]
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
>     q'              = isDelim (allowed (fromMaybe all ss)) l
>     continueWith  q = unlit' (ss <|> infer q') q ls
>     continue        = continueWith q
>     blockOpen     l = maybeToList l ++ continueWith q'
>     blockContinue l = l : continue
>     blockClose      = T.empty : continueWith Nothing
>     spurious      q = error ("at line " ++ show n ++ ": spurious " ++ show q)



What do we want `relit` to do?
==============================

Sadly, no, `relit` won't be able to take source code and
automatically convert it to literate code. I'm not quite up to the
challenge of automatically generating meaningful documentation from
arbitrary code... I wish I was.

What `relit` will do is read a literate file using one style of
delimiters and emit the same file using an other style of delimiters.

> relit :: Maybe Style -> Name -> [(Int, Text)] -> [Text]
> relit ss ts = relit' ss ts Nothing

Again, we will interpret the helper function `relit'` as an
automaton, which remembers the current state. However, we now also
need a function which can emit code blocks in a certain style. For
this purpose we will define a triple of functions.

> emitBirdTag :: Text -> Text
> emitBirdTag l = "> " `T.append` l
>
> emitOpen  :: Name -> Maybe Text -> [Text]
> emitOpen  Bird     l = T.empty       : map emitBirdTag (maybeToList l)
> emitOpen  Markdown l = backtickFence : maybeToList l
> emitOpen  _        l = beginCode     : maybeToList l
>
> emitCode  :: Name -> Text -> Text
> emitCode  Bird     l = emitBirdTag l
> emitCode  _        l = l
>
> emitClose :: Name -> Text
> emitClose Bird       = T.empty
> emitClose Markdown   = backtickFence
> emitClose _          = endCode

Using these simple functions we can easily define the `relit'`
function.

> relit' :: Maybe Style -> Name -> State -> [(Int, Text)] -> [Text]
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
>     q'              = isDelim (allowed (fromMaybe all ss)) l
>     continueWith  q = relit' (ss <|> infer q') ts q ls
>     continue        = continueWith q
>     blockOpen     l = emitOpen  ts l ++ continueWith q'
>     blockContinue l = emitCode  ts l : continue
>     blockClose      = emitClose ts   : continueWith Nothing
>     spurious      q = error ("at line " ++ show n ++ ": spurious " ++ show q)



Implementing the `main` function
================================

All that remains now is to implement the `main` function. This is
grossly uninteresting, so go look elsewhere.

> str2name :: String -> Maybe Name
> str2name arg = case map toLower arg of
>   "latex"    -> Just LaTeX
>   "bird"     -> Just Bird
>   "markdown" -> Just Markdown
>   "code"     -> Nothing
>   _          -> error ("non-existent style " ++ arg)
>
> name2style All      = all
> name2style LaTeX    = latex
> name2style Bird     = bird
> name2style Markdown = markdown
>
> data Options = Options
>   { optSourceStyle :: Maybe Style
>   , optTargetStyle :: Maybe Name
>   , optInputFile   :: IO Text
>   , optOutputFile  :: Text -> IO ()
>   }
>
> defaultOptions :: Options
> defaultOptions = Options
>   { optSourceStyle = Nothing
>   , optTargetStyle = Nothing
>   , optInputFile   = T.getContents
>   , optOutputFile  = T.putStrLn
>   }
>
> options :: [ OptDescr (Options -> IO Options) ]
> options =
>   [ Option "s" ["source"]
>     (ReqArg (\arg opt -> return opt { optSourceStyle = fmap name2style (str2name arg) })
>             "STYLE_NAME")
>     "Source style (latex, bird, markdown)"
>   , Option "t" ["target"]
>     (ReqArg (\arg opt -> return opt { optTargetStyle = str2name arg })
>             "STYLE_NAME")
>     "Target style (latex, bird, markdown)"
>   , Option "i" ["input"]
>     (ReqArg (\arg opt -> return opt { optInputFile = T.readFile arg })
>             "FILE")
>     "Input file (optional)"
>   , Option "o" ["output"]
>     (ReqArg (\arg opt -> return opt { optOutputFile = T.writeFile arg })
>             "FILE")
>     "Output file (optional)"
>   , Option "h" ["help"]
>     (NoArg  (\_ -> do
>     	        prg <- getProgName
>               hPutStrLn stderr (usageInfo prg options)
>               exitWith ExitSuccess))
>     "Show help"
>   ]
>
> main :: IO ()
> main = do
>   args <- getArgs
>
>   -- parse options
>   let (actions, nonOptions, errors) = getOpt Permute options args
>   opts <- foldl (>>=) (return defaultOptions) actions
>   let Options { optSourceStyle = ss
>               , optTargetStyle = ts
>               , optInputFile   = input
>               , optOutputFile  = output } = opts
>
>   -- define unlit/relit
>   let run = maybe (unlit ss) (relit ss) ts
>
>   -- run unlit/relit
>   input >>= output . T.unlines . run . zip [1..] . T.lines


[^fenced-code-attributes]: http://johnmacfarlane.net/pandoc/demo/example9/pandocs-markdown.html#extension-fenced_code_attributes
