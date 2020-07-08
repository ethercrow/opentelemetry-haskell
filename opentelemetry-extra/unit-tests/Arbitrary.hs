{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Arbitrary where

import Data.Function
import qualified Data.Text as T
import OpenTelemetry.Common
import OpenTelemetry.Eventlog
import OpenTelemetry.EventlogStreaming_Internal
import OpenTelemetry.SpanContext
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Instances.ByteString()
import TextShow

newtype TextWithout0 = TextWithout0 T.Text

instance Arbitrary TextWithout0 where
  arbitrary =
    arbitrary >>= \s ->
      s
        & map (\c -> if c == '\0' then ' ' else c)
        & (<> "_")
        & T.pack
        & TextWithout0
        & pure

deriving via TextWithout0 instance Arbitrary SpanName

deriving via TextWithout0 instance Arbitrary TagName

deriving via TextWithout0 instance Arbitrary TagVal

deriving via TextWithout0 instance Arbitrary EventName

deriving via TextWithout0 instance Arbitrary EventVal

deriving instance Arbitrary SpanInFlight

deriving instance Arbitrary SpanId

deriving instance Arbitrary TraceId

instance Arbitrary SomeInstrument where
  arbitrary = oneof
    [ SomeInstrument <$> (Counter <$> arbitrary <*> arbitrary)
    , SomeInstrument <$> (UpDownCounter <$> arbitrary <*> arbitrary)
    , SomeInstrument <$> (ValueRecorder <$> arbitrary <*> arbitrary)
    , SomeInstrument <$> (SumObserver <$> arbitrary <*> arbitrary)
    , SomeInstrument <$> (UpDownSumObserver <$> arbitrary <*> arbitrary)
    , SomeInstrument <$> (ValueObserver <$> arbitrary <*> arbitrary)
    ]

instance Arbitrary SpanContext where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary InstrumentType where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary OpenTelemetryEventlogEvent where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance TextShow SpanInFlight
