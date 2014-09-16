```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
```

The `bird2tex` executable reads a literate file in Bird-style from the
standard input, and emits a literate file in LaTeX-style to the standard
output. It does this by calling the `bird2tex` function and printing the
result.

```haskell
main :: IO ()
main = BS.getContents >>=
       sequence_ . map BS.putStrLn . bird2tex False . BS.lines
```

The `bird2tex` function is best seen as an automaton with two states:
either it *is* is a code block, or it *isn't*. To represent this state
we will use booleans.

```haskell
type State = Bool
```

The automaton uses its state to distinguish whether it is currently
entering a code block---whereupon it changes its state to `True` and
emits a `\begin{code}` tag---or exiting a code block---whereupon it
changes its state to `False` and emits an `\end{code}` tag. In all other
cases, the automaton simply copies the line verbatim, stripping bird
tags where necessary.

```haskell
bird2tex :: State -> [ByteString] -> [ByteString]
bird2tex _ [] = []
bird2tex False (l:ls) -- outside of code block
  | isBirdTag l = "\\begin{code}" : stripBirdTag l : bird2tex True ls
  | otherwise   = l : bird2tex False ls
bird2tex True  (l:ls) -- inside of code block
  | isBirdTag l = stripBirdTag l : bird2tex True ls
  | otherwise   = "\\end{code}" : l : bird2tex False ls
```

A bird tag is defined as *either* a line containing solely the symbol
'\>', or a line starting with the symbol '\>' followed by at least one
space.

```haskell
isBirdTag :: ByteString -> Bool
isBirdTag l = (l == ">") || ("> " `BS.isPrefixOf` l)
```

Due to this definition, whenever we strip a bird tag, we also remove a
the first space following it.

```haskell
stripBirdTag :: ByteString -> ByteString
stripBirdTag l
  | l == ">"  = ""
  | otherwise = BS.drop 2 l
```
