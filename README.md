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

Add `opentelemetry` to dependencies and sprinkle `withSpan` on interesting `IO` actions (or any other `m` satisfying `(MonadIO m, MonadCatch m)`.

See [megaexample](megaexample/README.md).

TODO: more examples

### As an application author

At the start of your application you configure the OpenTelemetry exporter. Here's the simplest way that exports to a file:

```haskell
import OpenTelemetry.FileExporter

main = do
  exporter <- createFileSpanExporter "my-application.trace.json"
  let otConfig = OpenTelemetryConfig { otcSpanExporter = exporter }
  withOpenTelemetry otConfig $ do
    ... the rest of the application ...
```


After you run your instrumented application, you'll have a `my-application.trace.json` file that you can load into Chrome's `about:tracing` page and get something like this:

![chrome_tracing_screenshot](https://i.imgur.com/q62yAkC.png)

Or load it into [Tracy](https://bitbucket.org/wolfpld/tracy/src/master/README.md), much more capable profile visualizer:

![tracy_screenshot](https://i.imgur.com/nbmma87.png)

Alternatively, configure the exporter to send tracing data to a compatible collector such as Jaeger, Zipkin, LightStep, HoneyComb, etc.

Explore the profile data to find performance problems and unexpected things, fix those, adjust instrumentation, repeat.

## How does it work?

TODO

## How do I contribute?

TODO

