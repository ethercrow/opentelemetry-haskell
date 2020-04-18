# opentelemetry-haskell

![Build](https://github.com/ethercrow/opentelemetry-haskell/workflows/Build/badge.svg)
![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/opentelemetry/badge)
![Hackage](https://img.shields.io/hackage/v/opentelemetry)
[![opentelemetry on Stackage Nightly](http://stackage.org/package/opentelemetry/badge/nightly)](http://stackage.org/nightly/package/opentelemetry)

## What is it?

The client library for [OpenTelemetry](https://opentelemetry.io).

The idea is to instrument your application code and configure where the telemetry data is sent, e.g. to a file or a service of your choice. This is like always-on debugging and profiling that helps you to understand the runtime behavior of your systems and spot and fix correctness and performance problems.

### Some more exposition

Long term utopic goal: every popular library doing networked interactions or heavy computations either is instrumented directly or has a companion library like `libfoo-with-opentelemetry` or at least a documented example. This way you could observe your typical industrial application doing HTTP, RPC, SQL, GraphQL, DAOSOAPCORBAXML and other enterprise things with minimal amount of effort spent on instrumentation.

Another utopic goal: Haskell development tools like stack, cabal, HIE, hlint, ormolu and maybe even ghc itself are instrumented so that we can all be aware what exactly is slow in the toolchain and improve it.

## How do I use it?

### As a library author

Add `opentelemetry` to dependencies, `import OpenTelemetry.Eventlog` and sprinkle `withSpan` on interesting `IO` actions (or any other `m` satisfying `(MonadIO m, MonadMask m)`:

```
module MyLib (avoidSuccess) where

import OpenTelemetry.Eventlog

avoidSuccess :: Costs -> IO ()
avoidSuccess costs = withSpan "MyLib.avoid_success" $ do
  addEvent "message" "Started avoiding success"
  setTag "costs" "all"
  addEvent "message" "Finished avoiding success"
  ...
  pure ()
```

For a comprehensive example see the [megaexample](megaexample/README.md) subproject.

### As an application author

1. Instrument interesting parts of the code, same as in the section above.
2. Compile your executable with `-eventlog`.
3. Run it with `+RTS -l -olmy_application.eventlog`.
4. Export the resulting eventlog to some program or service capable of trace data presentation, see [Exporters] below.
5. Explore the profile data to find performance problems and unexpected things, fix those, adjust instrumentation, repeat.

Note that currently 

## Exporters

TODO: Document how to export trace data to Chrome or Tracy.

### Zipkin

```
# Launch a Zipkin instance on localhost.
# See https://zipkin.io/pages/quickstart.html for a non-docker alternative.
docker run -p 9411:9411 openzipkin/zipkin-slim

# Export the eventlog.
eventlog-to-zipkin read my_application.eventlog
```

Open `http://localhost:9411/zipkin` in your browser.

### Jaeger

```
# Launch a Zipkin-compatible Jaeger service locally.
# Binaries and docker images are available at https://www.jaegertracing.io/docs/latest/getting-started/.
jaeger-all-in-one --collector.zipkin.http-port=9411

# Export the eventlog.
eventlog-to-zipkin read my_application.eventlog
```

Open `http://localhost:16686/search` in your browser.

### Honeycomb

```
# Launch a HoneyComb relay that accepts trace data in Zipkin format.
# https://docs.honeycomb.io/working-with-your-data/tracing/send-trace-data/#opentracing.
docker run -p 9411:9411 honeycombio/honeycomb-opentracing-proxy -k <my_api_key> -d traces

# Export the eventlog.
eventlog-to-zipkin read my_application.eventlog
```


### Lightstep

```
export LIGHTSTEP_TOKEN=<my_token>

# Export the eventlog
eventlog-to-lightstep read my_application.eventlog
```

Here is how a trace of `stack install` looks loaded in LightStep:

![lightstep_screenshot](https://i.imgur.com/fenCK7f.png)

## How does it work?

Please see the [OpenTelemetry documentation](https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/overview.md) for the general overview.

## How do I contribute?

Use it in your projects! Report what went well and especially what didn't go
well. If your usage pattern is unlike anything in the `megaexample` project,
consider adding it there.

When making a pull request for some user visible change please update the
CHANGELOG.md file accordingly.

