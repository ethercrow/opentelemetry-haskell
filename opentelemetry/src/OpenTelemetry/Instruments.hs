{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module OpenTelemetry.Instruments where

import qualified Data.Text as T

data Synchronicity = Synchronous | Asynchronous
data Additivity = Additive | NonAdditive
data Monotonicity = Monotonic | NonMonotonic

type InstrumentName = T.Text

type Counter t           = Instrument 'Synchronous  'Additive    'Monotonic t
type UpDownCounter t     = Instrument 'Synchronous  'Additive    'NonMonotonic t
type ValueRecorder t     = Instrument 'Synchronous  'NonAdditive 'NonMonotonic t
type SumObserver t       = Instrument 'Asynchronous 'Additive    'Monotonic t
type UpDownSumObserver t = Instrument 'Asynchronous 'Additive    'NonMonotonic t
type ValueObserver t     = Instrument 'Asynchronous 'NonAdditive 'NonMonotonic t

-- | An OpenTelemetry instrument as defined in the OpenTelemetry Metrics API
-- (https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/metrics/api.md)
data Instrument (s :: Synchronicity) (a :: Additivity) (m :: Monotonicity) t where
  Counter           :: InstrumentName -> Counter t
  UpDownCounter     :: InstrumentName -> UpDownCounter t
  ValueRecorder     :: InstrumentName -> ValueRecorder t
  SumObserver       :: InstrumentName -> SumObserver t
  UpDownSumObserver :: InstrumentName -> UpDownSumObserver t
  ValueObserver     :: InstrumentName -> ValueObserver t

data SomeInstrument t = forall s a m. SomeInstrument (Instrument s a m t)

deriving instance Show (Instrument s a m t)
deriving instance Eq (Instrument s a m t)

instance Show (SomeInstrument t) where
  show (SomeInstrument i) = show i

instance Eq (SomeInstrument t) where
  (SomeInstrument i1) == (SomeInstrument i2) = case (i1, i2) of
    (Counter s1, Counter s2) -> s1 == s2
    (UpDownCounter s1, UpDownCounter s2) -> s1 == s2
    (ValueRecorder s1, ValueRecorder s2) -> s1 == s2
    (SumObserver s1, SumObserver s2) -> s1 == s2
    (UpDownSumObserver s1, UpDownSumObserver s2) -> s1 == s2
    (ValueObserver s1, ValueObserver s2) -> s1 == s2
    (_, _) -> False


instrumentName :: Instrument s a m t -> InstrumentName
instrumentName (Counter n) = n
instrumentName (UpDownCounter n) = n
instrumentName (ValueRecorder n) = n
instrumentName (SumObserver n) = n
instrumentName (UpDownSumObserver n) = n
instrumentName (ValueObserver n) = n
