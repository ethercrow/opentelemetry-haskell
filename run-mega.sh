#!/usr/bin/env bash


FILE=${FILE:-megaexample.eventlog}
rm -f megaexample.eventlog megaexample.eventlog.trace.json megaexample.eventlog.tracy
env STACK_YAML="stack-8.10.yaml" stack install \
    && opentelemetry-megaexample +RTS -T -l -ol${FILE} \
    && eventlog-to-tracy ${FILE}
