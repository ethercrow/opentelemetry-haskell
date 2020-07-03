{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module OpenTelemetry.Metrics
  ( Synchronicity(..)
  , Additivity(..)
  , Monotonicity(..)
  , Counter
  , UpDownCounter
  , ValueRecorder
  , SumObserver
  , UpDownSumObserver
  , ValueObserver
  , Instrument(..)
  , SomeInstrument(..)
  , instrumentName
  , showInstrumentType
  , readInstrumentType
  , add
  , record
  , observe
  ) where

import qualified Data.Text as T
import Debug.Trace
import Text.Printf
import Control.Monad.IO.Class

data Synchronicity = Synchronous | Asynchronous
data Additivity = Additive | NonAdditive
data Monotonicity = Monotonic | NonMonotonic

type InstrumentName = T.Text

type Counter           = Instrument 'Synchronous  'Additive    'Monotonic
type UpDownCounter     = Instrument 'Synchronous  'Additive    'NonMonotonic
type ValueRecorder     = Instrument 'Synchronous  'NonAdditive 'NonMonotonic
type SumObserver       = Instrument 'Asynchronous 'Additive    'Monotonic
type UpDownSumObserver = Instrument 'Asynchronous 'Additive    'NonMonotonic
type ValueObserver     = Instrument 'Asynchronous 'NonAdditive 'NonMonotonic

-- | An OpenTelemetry instrument as defined in the OpenTelemetry Metrics API
-- (https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/metrics/api.md)
data Instrument (s :: Synchronicity) (a :: Additivity) (m :: Monotonicity) where
  Counter           :: InstrumentName -> Counter
  UpDownCounter     :: InstrumentName -> UpDownCounter
  ValueRecorder     :: InstrumentName -> ValueRecorder
  SumObserver       :: InstrumentName -> SumObserver
  UpDownSumObserver :: InstrumentName -> UpDownSumObserver
  ValueObserver     :: InstrumentName -> ValueObserver

data SomeInstrument = forall s a m. SomeInstrument (Instrument s a m)

instrumentName :: Instrument s a m -> InstrumentName
instrumentName (Counter n) = n
instrumentName (UpDownCounter n) = n
instrumentName (ValueRecorder n) = n
instrumentName (SumObserver n) = n
instrumentName (UpDownSumObserver n) = n
instrumentName (ValueObserver n) = n

showInstrumentType :: Instrument s a m -> String
showInstrumentType Counter{} = "Counter"
showInstrumentType UpDownCounter{} = "UpDownCounter"
showInstrumentType ValueRecorder{} = "ValueRecorder"
showInstrumentType SumObserver{} = "SumObserver"
showInstrumentType UpDownSumObserver{} = "UpDownSumObserver"
showInstrumentType ValueObserver{} = "ValueObserver"

readInstrumentType :: String -> Maybe (InstrumentName -> SomeInstrument)
readInstrumentType "Counter" = Just $ SomeInstrument . Counter
readInstrumentType "UpDownCounter" = Just $ SomeInstrument . UpDownCounter
readInstrumentType "ValueRecorder" = Just $ SomeInstrument . ValueRecorder
readInstrumentType "SumObserver" = Just $ SomeInstrument . SumObserver
readInstrumentType "UpDownSumObserver" = Just $ SomeInstrument . UpDownSumObserver
readInstrumentType "ValueObserver" = Just $ SomeInstrument . ValueObserver
readInstrumentType _ = Nothing

capture :: Instrument s a m -> Int -> IO ()
capture instrument v = liftIO . traceEventIO
  $ printf "ot2 metric %s %s %s" (showInstrumentType instrument) (instrumentName instrument) (show v)

-- | Take a measurement for a synchronous, additive instrument ('Counter', 'UpDowncounter')
add :: Instrument 'Synchronous 'Additive m -> Int -> IO ()
add = capture

-- | Take a measurement for a synchronous, non-additive instrument ('ValueRecorder')
record :: Instrument 'Synchronous 'NonAdditive m -> Int -> IO ()
record = capture

-- | Take a measurement for an asynchronous instrument ('SumObserver', 'UpDownSumObserver', 'ValueObserver')
observe :: Instrument 'Asynchronous a m -> Int -> IO ()
observe = capture

deriving instance Show (Instrument s a m)
deriving instance Eq (Instrument s a m)

instance Show SomeInstrument where
  show (SomeInstrument i) = show i

instance Eq SomeInstrument where
  (SomeInstrument i1) == (SomeInstrument i2) = case (i1, i2) of
    (Counter s1, Counter s2) -> s1 == s2
    (UpDownCounter s1, UpDownCounter s2) -> s1 == s2
    (ValueRecorder s1, ValueRecorder s2) -> s1 == s2
    (SumObserver s1, SumObserver s2) -> s1 == s2
    (UpDownSumObserver s1, UpDownSumObserver s2) -> s1 == s2
    (ValueObserver s1, ValueObserver s2) -> s1 == s2
    (_, _) -> False

