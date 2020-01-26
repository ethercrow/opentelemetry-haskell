# opentelemetry-haskell

![Build](https://github.com/ethercrow/opentelemetry-haskell/workflows/Build/badge.svg)
![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/opentelemetry/badge)
![Hackage](https://img.shields.io/hackage/v/lightstep-haskell)
[![lightstep-haskell on Stackage Nightly](http://stackage.org/package/lightstep-haskell/badge/nightly)](http://stackage.org/nightly/package/lightstep-haskell)

## What is it?

The client library for [OpenTelemetry](https://opentelemetry.io).

The idea is to instrument your application code and configure where the telemetry data is sent, e.g. to a file or a service of your choice. This is like always-on debugging and profiling that helps you to understand the runtime behavior of your systems and spot and fix correctness and performance problems.

## How do I use it?

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


After you run your instrumented application, you'll have a `my-app.trace.json` file that you can load into Chrome's `about:tracing` page and get something like this:

![chrome_tracing_screenshot](https://i.imgur.com/q62yAkC.png)

Alternatively, configure the exporter to send tracing data to a compatible collector such as Jaeger, Zipkin, LightStep, HoneyComb, etc.

### As a library author

## How does it work?

## How do I contribute?
