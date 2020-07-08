{-|

This module implements the instruments of the metrics portion of the
OpenTelemetry API. It is reexported by "OpenTelemetry.Eventlog" and should be
used by importing that.

The way to use the 'Instrument' type is throught the 'add', 'record' or
'observe' functions (depending on the instrument type) which capture metrics on
a given instrument.

Usage:

@
import OpenTelemetry.Eventlog

aCounter :: Counter
aCounter = Counter "myCounter"

anObserver :: ValueObserver
anObserver = ValueObserver "myObserver"

main :: IO ()
main = do
  add aCounter 3
  record anObserver 40
@

-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module OpenTelemetry.Metrics
  ( Instrument(..)
  , SomeInstrument(..)
  -- * Synonyms for specific types of Instrument
  , Counter
  , UpDownCounter
  , ValueRecorder
  , SumObserver
  , UpDownSumObserver
  , ValueObserver
  -- * Used for indexing Instrument. All possible combinations are covered
  , Synchronicity(..)
  , Additivity(..)
  , Monotonicity(..)
  , InstrumentName
  , InstrumentId
  , instrumentName
  , instrumentId
  , instrumentTag
  , readInstrumentTag
  , instrumentTagStr
  , readInstrumentTagStr
  , additive
  ) where

import Data.Hashable (Hashable(..))
import Data.Int
import Data.ByteString as BS
import Data.Word

data Synchronicity = Synchronous | Asynchronous
data Additivity = Additive | NonAdditive
data Monotonicity = Monotonic | NonMonotonic

type InstrumentName = BS.ByteString
type InstrumentId = Word64

type Counter           = Instrument 'Synchronous  'Additive    'Monotonic
type UpDownCounter     = Instrument 'Synchronous  'Additive    'NonMonotonic
type ValueRecorder     = Instrument 'Synchronous  'NonAdditive 'NonMonotonic
type SumObserver       = Instrument 'Asynchronous 'Additive    'Monotonic
type UpDownSumObserver = Instrument 'Asynchronous 'Additive    'NonMonotonic
type ValueObserver     = Instrument 'Asynchronous 'NonAdditive 'NonMonotonic

-- TODO: Support tags

-- | An OpenTelemetry instrument as defined in the OpenTelemetry Metrics API
-- (<https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/metrics/api.md>)
data Instrument (s :: Synchronicity) (a :: Additivity) (m :: Monotonicity) where
  Counter           :: InstrumentName -> InstrumentId -> Counter
  UpDownCounter     :: InstrumentName -> InstrumentId -> UpDownCounter
  ValueRecorder     :: InstrumentName -> InstrumentId -> ValueRecorder
  SumObserver       :: InstrumentName -> InstrumentId -> SumObserver
  UpDownSumObserver :: InstrumentName -> InstrumentId -> UpDownSumObserver
  ValueObserver     :: InstrumentName -> InstrumentId -> ValueObserver

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

instrumentTag :: Instrument s a m -> Int8
instrumentTag Counter{} = 1
instrumentTag UpDownCounter{} = 2
instrumentTag ValueRecorder{} = 3
instrumentTag SumObserver{} = 4
instrumentTag UpDownSumObserver{} = 5
instrumentTag ValueObserver{} = 6

readInstrumentTag :: Int8 -> Maybe (InstrumentName -> InstrumentId -> SomeInstrument)
readInstrumentTag 1 = Just $ fmap SomeInstrument . Counter
readInstrumentTag 2 = Just $ fmap SomeInstrument . UpDownCounter
readInstrumentTag 3 = Just $ fmap SomeInstrument . ValueRecorder
readInstrumentTag 4 = Just $ fmap SomeInstrument . SumObserver
readInstrumentTag 5 = Just $ fmap SomeInstrument . UpDownSumObserver
readInstrumentTag 6 = Just $ fmap SomeInstrument . ValueObserver
readInstrumentTag _ = Nothing

instrumentTagStr :: Instrument s a m -> String
instrumentTagStr Counter{} = "Counter"
instrumentTagStr UpDownCounter{} = "UpDownCounter"
instrumentTagStr ValueRecorder{} = "ValueRecorder"
instrumentTagStr SumObserver{} = "SumObserver"
instrumentTagStr UpDownSumObserver{} = "UpDownSumObserver"
instrumentTagStr ValueObserver{} = "ValueObserver"

readInstrumentTagStr :: String -> Maybe (InstrumentName -> InstrumentId -> SomeInstrument)
readInstrumentTagStr "Counter" = Just $ fmap SomeInstrument . Counter
readInstrumentTagStr "UpDownCounter" = Just $ fmap SomeInstrument . UpDownCounter
readInstrumentTagStr "ValueRecorder" = Just $ fmap SomeInstrument . ValueRecorder
readInstrumentTagStr "SumObserver" = Just $ fmap SomeInstrument . SumObserver
readInstrumentTagStr "UpDownSumObserver" = Just $ fmap SomeInstrument . UpDownSumObserver
readInstrumentTagStr "ValueObserver" = Just $ fmap SomeInstrument . ValueObserver
readInstrumentTagStr _ = Nothing

additive :: SomeInstrument -> Bool
additive (SomeInstrument i) = case i of
  Counter{} -> True
  UpDownCounter{} -> True
  ValueRecorder{} -> False
  SumObserver{} -> True
  UpDownSumObserver{} -> True
  ValueObserver{} -> False

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
      constructorIdx Counter{} = 0
      constructorIdx UpDownCounter{} = 1
      constructorIdx ValueRecorder{} = 2
      constructorIdx SumObserver{} = 3
      constructorIdx UpDownSumObserver{} = 4
      constructorIdx ValueObserver{} = 5

instance Hashable SomeInstrument where
  hashWithSalt s (SomeInstrument i) = hashWithSalt s i
