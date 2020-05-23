#!/bin/bash

BUILD_OPTIONS=""
case "$1" in
    -h|--help)
        cat<<EOF
Builds, runs mega example and shows charts.

  -b --binary
  use lazy ByteString instead String

EOF
        exit 1;;
    -b|--binary)
        BUILD_OPTIONS="--flag megaexample:binary"
        ;;
    "") ;;
    *)
        echo "Bad option [$@] see --help"
        exit 1;;
esac

FILE=${FILE:-megaexample.eventlog}
rm -f megaexample.eventlog megaexample.eventlog.trace.json megaexample.eventlog.tracy
stack install $BUILD_OPTIONS \
    && opentelemetry-megaexample +RTS -T -l -ol${FILE} \
    && eventlog-to-tracy ${FILE}
