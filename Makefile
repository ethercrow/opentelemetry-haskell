
.PHONY: format
format:
	find . -name '*.hs' -exec ormolu --ghc-opt -XBangPatterns --mode inplace '{}' \;

.PHONY: pedantic-build
pedantic-build:
	stack clean
	stack build --ghc-options "-Werror"
