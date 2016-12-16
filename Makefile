all: src/Unlit/String.hs README.md

test: test/ghcunlit
	runhaskell test/TestUnlit.hs

test/ghcunlit: test/ghcunlit.c
	gcc -O2 -o test/ghcunlit test/ghcunlit.c

dist: src/Main.hs src/Unlit/Text.lhs src/Unlit/String.hs
	cabal configure
	cabal sdist

build: src/Main.hs src/Unlit/Text.lhs src/Unlit/String.hs
	cabal configure
	cabal build

install: src/Main.hs src/Unlit/Text.lhs src/Unlit/String.hs
	cabal configure
	cabal install

README.md: Makefile src/Unlit/Text.lhs
	cat src/Unlit/Text.lhs \
	| unlit --language haskell -t backtickfence -l haskell \
	| sed '1i [![Build Status](https://travis-ci.org/pepijnkokke/unlit.png?branch=master)](https://travis-ci.org/pepijnkokke/unlit)' \
	> README.md

src/Unlit/String.hs: Makefile src/Unlit/Text.lhs
	cat src/Unlit/Text.lhs                                                            \
	| unlit -f bird -t code                                                           \
	| sed '1d;2d;17d;18d'                                                             \
	| sed 's/Text/String/g;s/pack//g'                                                 \
	| sed '15i import Prelude hiding \(all, or\)'                                     \
	| sed '16i import Data.List \(isPrefixOf, isInfixOf, isSuffixOf, dropWhileEnd\)'  \
	| sed '17i import qualified Data.Char as Char\n'                                  \
	| sed '19i stripStart, stripEnd, toLower :: String -> String'                     \
	| sed '20i stripStart = dropWhile Char.isSpace'                                   \
	| sed '21i stripEnd   = dropWhileEnd Char.isSpace'                                \
	| sed '22i toLower    = map Char.toLower'                                         \
	> src/Unlit/String.hs

.PHONY: test dist build install
