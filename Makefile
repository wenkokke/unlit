STYLES=latex backtickfence tildefence orgmode jekyll orgmode markdown asciidoc

all: src/Unlit/String.hs README.md

test: test/ghcunlit
	runhaskell test/TestUnlit.hs
	make roundtrip

roundtrip:
	unlit -l haskell -i src/Unlit/Text.lhs -o test/roundtrip.unlit || exit 1
	@for i in $(STYLES); do \
		echo "$$i"; \
		unlit -t $$i -l haskell -i src/Unlit/Text.lhs -o test/roundtrip.1 || exit 1; \
		unlit -i test/roundtrip.1 -o test/roundtrip.2 || exit 1; \
		diff test/roundtrip.2 test/roundtrip.unlit || exit 1; \
		unlit -f $$i -t bird -i test/roundtrip.1 -o test/roundtrip.2 || exit 1; \
		diff test/roundtrip.2 src/Unlit/Text.lhs || exit 1; \
		for j in $(STYLES); do \
			echo "  <-> $$j"; \
			unlit -f $$i -t $$j -i test/roundtrip.1 -o test/roundtrip.2 || exit 1; \
			unlit -f $$j -t $$i -i test/roundtrip.2 -o test/roundtrip.3 || exit 1; \
			diff test/roundtrip.1 test/roundtrip.3 || exit 1; \
		done; \
		echo "$$i inferred"; \
		unlit -t $$i -l haskell -i src/Unlit/Text.lhs -o test/roundtrip.1 || exit 1; \
		unlit -t bird -i test/roundtrip.1 -o test/roundtrip.2 || exit 1; \
		diff test/roundtrip.2 src/Unlit/Text.lhs || exit 1; \
		for j in $(STYLES); do \
			echo "  <-> $$j"; \
			unlit -t $$j -i test/roundtrip.1 -o test/roundtrip.2 || exit 1; \
			unlit -t $$i -i test/roundtrip.2 -o test/roundtrip.3 || exit 1; \
			diff test/roundtrip.1 test/roundtrip.3 || exit 1; \
		done; \
		echo "$$i without empty lines"; \
		sed '/^\s*$$/d' src/Unlit/Text.lhs > test/roundtrip.0; \
		unlit -t $$i -l haskell -i test/roundtrip.0 -o test/roundtrip.1 || exit 1; \
		unlit -f $$i -t bird -i test/roundtrip.1 -o test/roundtrip.2 || exit 1; \
		diff test/roundtrip.2 test/roundtrip.0 || exit 1; \
		for j in $(STYLES); do \
			echo "  <-> $$j"; \
			unlit -f $$i -t $$j -i test/roundtrip.1 -o test/roundtrip.2 || exit 1; \
			unlit -f $$j -t $$i -i test/roundtrip.2 -o test/roundtrip.3 || exit 1; \
			diff test/roundtrip.1 test/roundtrip.3 || exit 1; \
		done; \
		echo "$$i inferred without empty lines"; \
		sed '/^\s*$$/d' src/Unlit/Text.lhs > test/roundtrip.0; \
		unlit -t $$i -l haskell -i test/roundtrip.0 -o test/roundtrip.1 || exit 1; \
		unlit -t bird -i test/roundtrip.1 -o test/roundtrip.2 || exit 1; \
		diff test/roundtrip.2 test/roundtrip.0 || exit 1; \
		for j in $(STYLES); do \
			echo "  <-> $$j"; \
			unlit -t $$j -i test/roundtrip.1 -o test/roundtrip.2 || exit 1; \
			unlit -t $$i -i test/roundtrip.2 -o test/roundtrip.3 || exit 1; \
			diff test/roundtrip.1 test/roundtrip.3 || exit 1; \
		done \
	done

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
	| unlit -f bird                                                                   \
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

.PHONY: test roundtrip dist build install
