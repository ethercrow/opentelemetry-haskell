
.PHONY: build
build:
	cabal build all

.PHONY: format
format:
	find opentelemetry           -name '*.hs' -exec ormolu --ghc-opt -XBangPatterns --mode inplace '{}' \;
	find opentelemetry-extra     -name '*.hs' -exec ormolu --ghc-opt -XBangPatterns --mode inplace '{}' \;
	find opentelemetry-lightstep -name '*.hs' -exec ormolu --ghc-opt -XBangPatterns --mode inplace '{}' \;
	find opentelemetry-wai       -name '*.hs' -exec ormolu --ghc-opt -XBangPatterns --mode inplace '{}' \;

.PHONY: pedantic-build
pedantic-build:
	stack clean
	stack build --ghc-options "-Werror"

.PHONY: release
release: pedantic-build
	stack sdist
	stack upload opentelemetry
	stack upload opentelemetry-extra
	stack upload opentelemetry-wai
	stack upload opentelemetry-lightstep
