{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
--
-- This is an internal module. The public interface is re-exported by "OpenTelemetry.Eventlog"
--
-- This module implements the instruments of the metrics portion of the
-- OpenTelemetry API. It is reexported by "OpenTelemetry.Eventlog" and should be
-- used by importing that.
--
-- The way to use the 'Instrument' type is throught the 'add', 'record' or
-- 'observe' functions (depending on the instrument type) which capture metrics on
-- a given instrument.
--
-- Usage:
--
-- @
-- import OpenTelemetry.Eventlog
--
-- aCounter :: Counter
-- aCounter = Counter "myCounter"
--
-- anObserver :: ValueObserver
-- anObserver = ValueObserver "myObserver"
--
-- main :: IO ()
-- main = do
--   add aCounter 3
--   record anObserver 40
-- @
module OpenTelemetry.Metrics_Internal
  ( Instrument (..),
    SomeInstrument (..),

    -- * Synonyms for specific types of Instrument
    Counter,
    UpDownCounter,
    ValueRecorder,
    SumObserver,
    UpDownSumObserver,
    ValueObserver,

    -- * Used for indexing Instrument. All possible combinations are covered
    Synchronicity (..),
    Additivity (..),
    Monotonicity (..),
    InstrumentName,
    InstrumentId,
    instrumentName,
    instrumentId,
  )
where

import Data.ByteString as BS
import Data.Hashable (Hashable (..))
import Data.Word

data Synchronicity = Synchronous | Asynchronous

data Additivity = Additive | NonAdditive

data Monotonicity = Monotonic | NonMonotonic

type InstrumentName = BS.ByteString

type InstrumentId = Word64

type Counter = Instrument 'Synchronous 'Additive 'Monotonic

type UpDownCounter = Instrument 'Synchronous 'Additive 'NonMonotonic

type ValueRecorder = Instrument 'Synchronous 'NonAdditive 'NonMonotonic

type SumObserver = Instrument 'Asynchronous 'Additive 'Monotonic

type UpDownSumObserver = Instrument 'Asynchronous 'Additive 'NonMonotonic

type ValueObserver = Instrument 'Asynchronous 'NonAdditive 'NonMonotonic

-- TODO: Support tags

-- | An OpenTelemetry instrument as defined in the OpenTelemetry Metrics API
-- (<https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/metrics/api.md>)
data Instrument (s :: Synchronicity) (a :: Additivity) (m :: Monotonicity) where
  Counter :: InstrumentName -> InstrumentId -> Counter
  UpDownCounter :: InstrumentName -> InstrumentId -> UpDownCounter
  ValueRecorder :: InstrumentName -> InstrumentId -> ValueRecorder
  SumObserver :: InstrumentName -> InstrumentId -> SumObserver
  UpDownSumObserver :: InstrumentName -> InstrumentId -> UpDownSumObserver
  ValueObserver :: InstrumentName -> InstrumentId -> ValueObserver

-- | Existential wrapper for 'Instrument'. Use when the exact type of Instrument does not matter.
data SomeInstrument = forall s a m. SomeInstrument (Instrument s a m)

instrumentName :: Instrument s a m -> InstrumentName
instrumentName (Counter n _) = n
instrumentName (UpDownCounter n _) = n
instrumentName (ValueRecorder n _) = n
instrumentName (SumObserver n _) = n
instrumentName (UpDownSumObserver n _) = n
instrumentName (ValueObserver n _) = n

instrumentId :: Instrument s a m -> InstrumentId
instrumentId (Counter _ i) = i
instrumentId (UpDownCounter _ i) = i
instrumentId (ValueRecorder _ i) = i
instrumentId (SumObserver _ i) = i
instrumentId (UpDownSumObserver _ i) = i
instrumentId (ValueObserver _ i) = i

deriving instance Show (Instrument s a m)

deriving instance Eq (Instrument s a m)

instance Show SomeInstrument where
  show (SomeInstrument i) = show i

instance Eq SomeInstrument where
  (SomeInstrument i1) == (SomeInstrument i2) = case (i1, i2) of
    (Counter s1 id1, Counter s2 id2) -> s1 == s2 && id1 == id2
    (UpDownCounter s1 id1, UpDownCounter s2 id2) -> s1 == s2 && id1 == id2
    (ValueRecorder s1 id1, ValueRecorder s2 id2) -> s1 == s2 && id1 == id2
    (SumObserver s1 id1, SumObserver s2 id2) -> s1 == s2 && id1 == id2
    (UpDownSumObserver s1 id1, UpDownSumObserver s2 id2) -> s1 == s2 && id1 == id2
    (ValueObserver s1 id1, ValueObserver s2 id2) -> s1 == s2 && id1 == id2
    (_, _) -> False

instance Hashable (Instrument s a m) where
  hashWithSalt s i = s `hashWithSalt` (constructorIdx i) `hashWithSalt` (instrumentName i)
    where
      constructorIdx :: Instrument s a m -> Int
      constructorIdx Counter {} = 0
      constructorIdx UpDownCounter {} = 1
      constructorIdx ValueRecorder {} = 2
      constructorIdx SumObserver {} = 3
      constructorIdx UpDownSumObserver {} = 4
      constructorIdx ValueObserver {} = 5

instance Hashable SomeInstrument where
  hashWithSalt s (SomeInstrument i) = hashWithSalt s i
