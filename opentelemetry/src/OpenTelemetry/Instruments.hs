{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
module OpenTelemetry.Instruments where

data Synchronicity = Synchronous | Asynchronous
data Additivity = Additive | NonAdditive
data Monotonicity = Monotonic | NonMonotonic

type Counter t           = Instrument 'Synchronous  'Additive    'Monotonic t
type UpDownCounter t     = Instrument 'Synchronous  'Additive    'NonMonotonic t
type ValueRecorder t     = Instrument 'Synchronous  'NonAdditive 'NonMonotonic t
type SumObserver t       = Instrument 'Asynchronous 'Additive    'Monotonic t
type UpDownSumObserver t = Instrument 'Asynchronous 'Additive    'NonMonotonic t
type ValueObserver t     = Instrument 'Asynchronous 'NonAdditive 'NonMonotonic t

-- | An OpenTelemetry instrument as defined in the OpenTelemetry Metrics API
-- (https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/metrics/api.md)
data Instrument (s :: Synchronicity) (a :: Additivity) (m :: Monotonicity) t where
  Counter           :: String -> Counter t
  UpDownCounter     :: String -> UpDownCounter t
  ValueRecorder     :: String -> ValueRecorder t
  SumObserver       :: String -> SumObserver t
  UpDownSumObserver :: String -> UpDownSumObserver t
  ValueObserver     :: String -> ValueObserver t

add :: Instrument 'Synchronous 'Additive m t -> t -> IO ()
add (Counter _) _ = undefined
add (UpDownCounter _) _ = undefined

record :: Instrument 'Synchronous 'NonAdditive 'NonMonotonic t -> t -> IO ()
record (ValueRecorder _) _ = undefined

observe :: Instrument 'Asynchronous a m t -> t -> IO ()
observe (SumObserver _) _ = undefined
observe (UpDownSumObserver _) _ = undefined
observe (ValueObserver _) _ = undefined
instrumentName :: Instrument s a m t -> String
instrumentName (Counter n) = n
instrumentName (UpDownCounter n) = n
instrumentName (ValueRecorder n) = n
instrumentName (SumObserver n) = n
instrumentName (UpDownSumObserver n) = n
instrumentName (ValueObserver n) = n
