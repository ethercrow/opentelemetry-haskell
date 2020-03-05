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

Add `opentelemetry` to dependencies and sprinkle `withSpan` on interesting `IO` actions (or any other `m` satisfying `(MonadIO m, MonadCatch m)`:

```
module MyLib (avoidSuccess) where

import OpenTelemetry.Implicit

avoidSuccess :: Costs -> IO ()
avoidSuccess costs = withSpan "MyLib.avoid_success" $ do
  addEvent "message" "Started avoiding success"
  setTag "costs" (show costs)
  addEvent "message" "Finished avoiding success"
  pure ()
```

For a comprehensive example see the [megaexample](megaexample/README.md) subproject.

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

Alternatively, configure the exporter to send tracing data to a compatible collector such as Jaeger, Zipkin, LightStep, HoneyComb, etc. For example here is how a trace of `stack install` looks loaded in LightStep:

![lightstep_screenshot](https://i.imgur.com/fenCK7f.png)

Explore the profile data to find performance problems and unexpected things, fix those, adjust instrumentation, repeat.

## How does it work?

Please see the [OpenTelemetry documentation](https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/overview.md) for the general overview.

One interesting thing specific to this library is how it manages causal relationships of spans. For example here the `bar` span is inferred to be a child of the `foo` span:

```haskell
withSpan "foo" $ do    -- (1)
  ...
  withSpan "bar" $ do  -- (2)
    do_bar_things
  ...
```

Internally the library is maintaining a stack of spans per Haskell thread.

At line `(1)` the `foo` span is created and pushed onto the stack.
At line `(2)` the `bar` span is created. The top-most span in the stack, namely `foo`, is assigned as its parent. The `bar` span is pushed onto the stack.

This scheme doesn't work when something involving threads happens between `(1)` and `(2)`. In such cases the programmer should connect the spans explicitly like this:

```haskell
withSpan "foo" $ do    -- (1)
  Just foo_span <- getCurrentActiveSpan
  bar_worker <- async $ do
    withChildSpanOf foo_span "bar" $ do  -- (2)
      do_bar_things
  wait bar_worker
  ...
```

## How do I contribute?

Use it in your projects! Report what went well and especially what didn't go
well. If your usage pattern is unlike anything in the `megaexample` project,
consider adding it there.

When making a pull request for some user visible change please update the
CHANGELOG.md file accordingly.

