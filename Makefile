all: src/Unlit/String.hs README.md

test: test/ghcunlit
	runhaskell test/TestUnlit.hs

test/ghcunlit: test/ghcunlit.c
	gcc -O2 -o ghcunlit ghcunlit.c

dist: src/Main.hs src/Unlit/Text.lhs src/Unlit/String.hs
	cabal configure
	cabal sdist

build: src/Main.hs src/Unlit/Text.lhs src/Unlit/String.hs
	cabal configure
	cabal build

install: src/Main.hs src/Unlit/Text.lhs src/Unlit/String.hs
	cabal configure
	cabal install

README.md: src/Unlit/Text.lhs
	cat src/Unlit/Text.lhs \
	| unlit -t backtickfence -l haskell \
	| gsed '1i [![Build Status](https://travis-ci.org/pepijnkokke/unlit.png?branch=master)](https://travis-ci.org/pepijnkokke/unlit)' \
        > README.md

src/Unlit/String.hs: src/Unlit/Text.lhs
	cat src/Unlit/Text.lhs                                              \
	| unlit -f bird -t code                                             \
	| gsed '7d;14d;15d'                                                 \
	| gsed 's/Text/String/g;s/unpack/id/g;s/pack/id/g'                  \
	| gsed '7i import Prelude hiding \(all, or\)'                       \
	| gsed '8i import Data.List \(isPrefixOf, isInfixOf, isSuffixOf, dropWhileEnd\)'  \
	> src/Unlit/String.hs

.PHONY: test dist build install
