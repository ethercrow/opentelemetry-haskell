{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DerivingVia #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Arbitrary where

import OpenTelemetry.EventlogStreaming_Internal
import Data.Text as T
import qualified Data.List as L
import OpenTelemetry.Binary.Eventlog
import OpenTelemetry.Common
import OpenTelemetry.SpanContext
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import TextShow

newtype TextWithout0 = TextWithout0 T.Text

instance Arbitrary TextWithout0 where
    arbitrary = arbitrary >>= \s -> return . TextWithout0 . T.pack $ space0 s
        where
          space0 = L.map $ \c -> if c == '\0' then ' ' else c

deriving via TextWithout0 instance Arbitrary SpanName
deriving via TextWithout0 instance Arbitrary TagName
deriving via TextWithout0 instance Arbitrary TagVal
deriving via TextWithout0 instance Arbitrary EventName
deriving via TextWithout0 instance Arbitrary EventVal


deriving instance Arbitrary SpanInFlight
deriving instance Arbitrary SpanId
deriving instance Arbitrary TraceId

instance Arbitrary SpanContext where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary OpenTelemetryEventlogEvent where
    arbitrary = genericArbitrary
    shrink = genericShrink

deriving instance TextShow SpanInFlight
