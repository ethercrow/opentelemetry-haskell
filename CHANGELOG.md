# Revision history for opentelemetry

## 0.8.0 -- 2022-04-07

* Record original thread id in Chrome output

## 0.7.1 -- 2021-10-18

* Fix pid field missing from metric events in eventlog-to-chrome output.

## 0.7.0 -- 2021-06-05

* More ergonomic presentation of multi-threaded profiles in Tracy.
* Internal change in opentelemetry-extra: replace aeson with jsonifier.

## 0.6.1 -- 2020-08-28

* Bring back the support for GHC-8.6.

## 0.6.0 -- 2020-07-10

* Metrics API.

## 0.5.3 -- 2020-07-04

* Use random numbers for span ids.

## 0.5.2 -- 2020-07-03

* Bring back beginSpan and endSpan lost in refactoring.

## 0.5.1 -- 2020-07-03

* Update version bounds.

## 0.5.0 -- 2020-07-01

* Introduce binary eventlog messages.
* Abandon GHC 8.6 in order to be able to send binary eventlog messages.
* Reintroduce GC spans.
* Tag all spans with what time was spent in GC.
* Tracy exporter.

## 0.4.2 -- 2020-05-22

* Chrome exporter now exports events (the OpenTelemetry events, not eventlog events).

## 0.4.1 -- 2020-05-22

* `eventlog-to-* read` now works with eventlogs written by multiple capabilities.

## 0.4.0 -- 2020-05-11

* `beginSpan/endSpan` can now be called from different threads.

## 0.3.2 -- 2020-05-02

* Added experimental `beginSpecificSpan` and `endSpecificSpan` functions for instrumenting
  code with custom control flow, where a span can start in one thread and end in another.

## 0.3.1 -- 2020-05-01

* Fixed eventlog-to-chrome to handle span names containing punctuation.

## 0.3.0 -- 2020-04-23

* opentelemetry library now writes to eventlogs and keeps no state in the instrumented applications.
* eventlog-to-X executables are introduced to converst the eventlogs into something viewable.
* Propagation is now configurable to enable interop with other instrumentation like Zipkin B3 headers.

## 0.2.0 -- 2020-03-07

* Remove the notion of an empty span, getCurrentActiveSpan now returns (Maybe Span).
* OpenTelemetry.Explicit API is removed due to lack of usage.

## 0.1.0 -- 2020-03-04

* Add Lightstep trace exporter using Zipkin format.
* Automatically set error=true tag on exceptions.

## 0.0.0.2 -- 2020-02-01

* Fix missing span tags in Chrome trace viewer.

## 0.0.0.1 -- 2020-02-01

* Added `withZeroConfigOpenTelemetry` to make the library easy to start using.

## 0.0.0.0 -- 2020-01-24

* First version. Released on an unsuspecting world.
