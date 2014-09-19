{-# LANGUAGE OverloadedStrings #-}
module Unlit.String (unlit, relit,
                   Style, all, latex, bird, haskell, markdown,
                   tildefence, backtickfence,
                   Language, forLanguage) where


import Control.Applicative ((<|>))
import Control.Monad (msum)
import qualified Data.List as T
import Data.Maybe (maybe,maybeToList,listToMaybe,fromMaybe)
import Data.Monoid (mempty,(<>))
import Prelude as T hiding (or,all)

or :: [a] -> [a] -> [a]
xs `or` [] = xs
[] `or` ys = ys
xs `or` ys = xs

data Delim
  = BeginCode
  | EndCode
  | BirdTag
  | TildeFence    (Maybe Language)
  | BacktickFence (Maybe Language)
  deriving (Eq)

instance Show Delim where
  show  BeginCode        = "\\begin{code}"
  show  EndCode          = "\\end{code}"
  show  BirdTag          = ">"
  show (TildeFence l)    = "~~~" ++ (fromMaybe "" l)
  show (BacktickFence l) = "```" ++ (fromMaybe "" l)

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

type Language = String

tildeFence, backtickFence :: String
tildeFence    = "~~~"
backtickFence = "```"

isTildeFence, isBacktickFence :: Maybe Language -> String -> Bool
isTildeFence    lang l =
  tildeFence `T.isPrefixOf` l && (maybe True (`T.isInfixOf` l) lang)
isBacktickFence lang l =
  backtickFence `T.isPrefixOf` l && (maybe True (`T.isInfixOf` l) lang)

(?:) :: Bool -> a -> Maybe a
True  ?: x = Just x
False ?: _ = Nothing

isDelim :: [Delim] -> String -> Maybe Delim
isDelim ds l = msum (map (go l) ds)
  where
    go :: String -> Delim -> Maybe Delim
    go l  BeginCode           = isBeginCode l          ?: BeginCode
    go l  EndCode             = isEndCode l            ?: EndCode
    go l  BirdTag             = isBirdTag l            ?: BirdTag
    go l (TildeFence    lang) = isTildeFence lang l    ?: TildeFence lang
    go l (BacktickFence lang) = isBacktickFence lang l ?: BacktickFence lang

match :: Delim -> Delim -> Bool
match  BeginCode         EndCode                = True
match (TildeFence _)    (TildeFence Nothing)    = True
match (BacktickFence _) (BacktickFence Nothing) = True
match _                  _                      = False

type Style = [Delim]

latex, bird, markdown :: Style
bird             = [BirdTag]
latex            = [BeginCode, EndCode]
haskell          = latex ++ bird
tildefence       = [TildeFence Nothing]
backtickfence    = [BacktickFence Nothing]
markdown         = bird ++ tildefence ++ backtickfence
all              = latex ++ markdown

forLanguage :: Language -> Style -> Style
forLanguage l = map (setLanguage (Just l))

setLanguage :: Maybe Language -> Delim -> Delim
setLanguage l (TildeFence _)    = TildeFence l
setLanguage l (BacktickFence _) = BacktickFence l
setLanguage _  d                = d

infer :: Maybe Delim -> Style
infer  Nothing         = []
infer (Just BeginCode) = latex
infer (Just _)         = markdown

unlit :: Style -> String -> String
unlit ss = T.unlines . unlit' ss Nothing . zip [1..] . T.lines

type State = Maybe Delim

unlit' :: Style -> State -> [(Int, String)] -> [String]
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
    q'              = isDelim (ss `or` all) l
    continueWith  q = unlit' (ss `or` infer q') q ls
    continue        = continueWith q
    blockOpen     l = maybeToList l ++ continueWith q'
    blockContinue l = l : continue
    blockClose      = mempty : continueWith Nothing
    spurious      q = error ("at line " ++ show n ++ ": spurious " ++ show q)

relit :: Style -> Style -> String -> String
relit ss ts = T.unlines . relit' ss (head ts) Nothing . zip [1..] . T.lines

emitBirdTag :: String -> String
emitBirdTag l = "> " <> l

emitOpen :: Delim -> Maybe String -> [String]
emitOpen BirdTag l = mempty    : map emitBirdTag (maybeToList l)
emitOpen EndCode l = beginCode : maybeToList l
emitOpen del     l = show del  : maybeToList l

emitCode :: Delim -> String -> String
emitCode BirdTag l = emitBirdTag l
emitCode _       l = l

emitClose :: Delim -> String
emitClose BirdTag   = mempty
emitClose BeginCode = endCode
emitClose del       = show (setLanguage Nothing del)

relit' :: Style -> Delim -> State -> [(Int, String)] -> [String]
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
    q'              = isDelim (ss `or` all) l
    continueWith  q = relit' (ss `or` infer q') ts q ls
    continue        = continueWith q
    blockOpen     l = emitOpen  ts l ++ continueWith q'
    blockContinue l = emitCode  ts l : continue
    blockClose      = emitClose ts   : continueWith Nothing
    spurious      q = error ("at line " ++ show n ++ ": spurious " ++ show q)
