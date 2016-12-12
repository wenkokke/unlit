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
	| unlit -t backtickfence -l haskell \
	| sed '1i [![Build Status](https://travis-ci.org/pepijnkokke/unlit.png?branch=master)](https://travis-ci.org/pepijnkokke/unlit)' \
        > README.md

src/Unlit/String.hs: Makefile src/Unlit/Text.lhs
	cat src/Unlit/Text.lhs                                              \
	| unlit -f bird -t code                                             \
	| sed '15d;16d'                                                     \
	| sed 's/Text/String/g;s/pack//g'   							                  \
	| sed '15i import Prelude hiding \(all, or\)'                        \
	| sed '16i import Data.List \(isPrefixOf, isInfixOf, isSuffixOf, dropWhileEnd\)'  \
	| sed '17i import Data.Char \(isSpace\)'                      \
	| sed '18i stripStart, stripEnd :: String -> String'  				\
	| sed '19i stripStart = dropWhile isSpace'  									\
  | sed '20i stripEnd = dropWhileEnd isSpace'										\
	> src/Unlit/String.hs

.PHONY: test dist build install
