# Revision history for opentelemetry

## 0.3.0 -- In development

* Restructuring of libraries
* Propagation is now configurable to enable interop with other instrumentation like Zipkin B3 headers.

## 0.2.0 -- 2020-03-07

* Remove the notion of an empty span, getCurrentActiveSpan now returns (Maybe Span)
* OpenTelemetry.Explicit API is removed due to lack of usage

## 0.1.0 -- 2020-03-04

* Add Lightstep trace exporter using Zipkin format
* Automatically set error=true tag on exceptions

## 0.0.0.2 -- 2020-02-01

* Fix missing span tags in Chrome trace viewer

## 0.0.0.1 -- 2020-02-01

* Added `withZeroConfigOpenTelemetry` to make the library easy to start using

## 0.0.0.0 -- 2020-01-24

* First version. Released on an unsuspecting world.
