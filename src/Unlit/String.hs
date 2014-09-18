{-# LANGUAGE OverloadedStrings #-}
module Unlit.String (unlit, relit, Style(..), Name(..), name2style) where

import Prelude hiding (all)
import Control.Applicative ((<|>))
import Data.Maybe (maybeToList,listToMaybe,fromMaybe)
import qualified Data.List as T

data Delim = BeginCode  | EndCode
           | BirdTag
           | TildeFence | BacktickFence
           deriving (Eq)

instance Show Delim where
  show BeginCode     = "\\begin{code}"
  show EndCode       = "\\end{code}"
  show BirdTag       = ">"
  show TildeFence    = "~~~"
  show BacktickFence = "```"

beginCode, endCode :: String
beginCode = "\\begin{code}"
endCode   = "\\end{code}"

isBeginCode, isEndCode :: String -> Bool
isBeginCode l = beginCode `T.isPrefixOf` l
isEndCode   l = endCode   `T.isPrefixOf` l

isBirdTag :: String -> Bool
isBirdTag l = (l == ">") || ("> " `T.isPrefixOf` l)

stripBirdTag :: String -> String
stripBirdTag l
  | l == ">" = ""
  | otherwise = T.drop 2 l

tildeFence, backtickFence :: String
tildeFence    = "~~~"
backtickFence = "```"

isTildeFence, isBacktickFence :: String -> Bool
isTildeFence    l = tildeFence    `T.isPrefixOf` l
isBacktickFence l = backtickFence `T.isPrefixOf` l

isDelim :: [Delim] -> String -> Maybe Delim
isDelim ds l =
  listToMaybe . map fst . filter (\(d,p) -> d `elem` ds && p l) $ detectors
  where
    detectors :: [(Delim, String -> Bool)]
    detectors =
      [ (BeginCode     , isBeginCode)
      , (EndCode       , isEndCode)
      , (BirdTag       , isBirdTag)
      , (TildeFence    , isTildeFence)
      , (BacktickFence , isBacktickFence)
      ]

match :: Delim -> Delim -> Bool
match BeginCode     EndCode       = True
match TildeFence    TildeFence    = True
match BacktickFence BacktickFence = True
match _             _             = False

data Name  = All | Bird | Haskell | LaTeX | Markdown deriving (Show)
data Style = Style { name :: Name, allowed :: [Delim] }

latex, bird, markdown :: Style
all      = Style All      [BeginCode, EndCode, BirdTag, TildeFence, BacktickFence]
bird     = Style Bird     [BirdTag]
haskell  = Style Haskell  [BeginCode, EndCode, BirdTag]
latex    = Style LaTeX    [BeginCode, EndCode]
markdown = Style Markdown [BirdTag, TildeFence, BacktickFence]

name2style All      = all
name2style Bird     = bird
name2style Haskell  = haskell
name2style LaTeX    = latex
name2style Markdown = markdown

infer :: Maybe Delim -> Maybe Style
infer  Nothing         = Nothing
infer (Just BeginCode) = Just latex
infer (Just _)         = Just markdown

unlit :: Maybe Style -> [(Int, String)] -> [String]
unlit ss = unlit' ss Nothing

type State = Maybe Delim

unlit' :: Maybe Style -> State -> [(Int, String)] -> [String]
unlit' _ _ [] = []
unlit' ss q ((n, l):ls) = case (q, q') of

  (Nothing      , Nothing)      -> continue
  (Nothing      , Just BirdTag) -> blockOpen     $ Just (stripBirdTag l)
  (Just BirdTag , Just BirdTag) -> blockContinue $ stripBirdTag l
  (Just BirdTag , Nothing)      -> blockClose
  (Nothing      , Just EndCode) -> spurious EndCode
  (Nothing      , Just o)       -> blockOpen     $ Nothing
  (Just o       , Nothing)      -> blockContinue $ l
  (Just o       , Just c)       -> if o `match` c then blockClose else spurious c

  where
    q'              = isDelim (allowed (fromMaybe all ss)) l
    continueWith  q = unlit' (ss <|> infer q') q ls
    continue        = continueWith q
    blockOpen     l = maybeToList l ++ continueWith q'
    blockContinue l = l : continue
    blockClose      = "" : continueWith Nothing
    spurious      q = error ("at line " ++ show n ++ ": spurious " ++ show q)

relit :: Maybe Style -> Name -> [(Int, String)] -> [String]
relit ss ts = relit' ss ts Nothing

emitBirdTag :: String -> String
emitBirdTag l = "> " ++ l

emitOpen  :: Name -> Maybe String -> [String]
emitOpen  Bird     l = ""            : map emitBirdTag (maybeToList l)
emitOpen  Markdown l = backtickFence : maybeToList l
emitOpen  _        l = beginCode     : maybeToList l

emitCode  :: Name -> String -> String
emitCode  Bird     l = emitBirdTag l
emitCode  _        l = l

emitClose :: Name -> String
emitClose Bird       = ""
emitClose Markdown   = backtickFence
emitClose _          = endCode

relit' :: Maybe Style -> Name -> State -> [(Int, String)] -> [String]
relit' _ _ _ [] = []
relit' ss ts q ((n, l):ls) = case (q, q') of

  (Nothing      , Nothing)      -> l : continue
  (Nothing      , Just BirdTag) -> blockOpen     $ Just (stripBirdTag l)
  (Just BirdTag , Just BirdTag) -> blockContinue $ stripBirdTag l
  (Just BirdTag , Nothing)      -> blockClose
  (Nothing      , Just EndCode) -> spurious EndCode
  (Nothing      , Just o)       -> blockOpen     $ Nothing
  (Just o       , Nothing)      -> blockContinue $ l
  (Just o       , Just c)       -> if o `match` c then blockClose else spurious c

  where
    q'              = isDelim (allowed (fromMaybe all ss)) l
    continueWith  q = relit' (ss <|> infer q') ts q ls
    continue        = continueWith q
    blockOpen     l = emitOpen  ts l ++ continueWith q'
    blockContinue l = emitCode  ts l : continue
    blockClose      = emitClose ts   : continueWith Nothing
    spurious      q = error ("at line " ++ show n ++ ": spurious " ++ show q)
