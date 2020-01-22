{- This file was auto-generated from collector.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies,
  UndecidableInstances, GeneralizedNewtypeDeriving,
  MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
  PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds,
  BangPatterns, TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports#-}
module Proto.Collector_Fields where
import qualified Data.ProtoLens.Runtime.Prelude as Prelude
import qualified Data.ProtoLens.Runtime.Data.Int as Data.Int
import qualified Data.ProtoLens.Runtime.Data.Monoid as Data.Monoid
import qualified Data.ProtoLens.Runtime.Data.Word as Data.Word
import qualified Data.ProtoLens.Runtime.Data.ProtoLens
       as Data.ProtoLens
import qualified
       Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Bytes
       as Data.ProtoLens.Encoding.Bytes
import qualified
       Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Growing
       as Data.ProtoLens.Encoding.Growing
import qualified
       Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Parser.Unsafe
       as Data.ProtoLens.Encoding.Parser.Unsafe
import qualified
       Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Wire
       as Data.ProtoLens.Encoding.Wire
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Field
       as Data.ProtoLens.Field
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Message.Enum
       as Data.ProtoLens.Message.Enum
import qualified
       Data.ProtoLens.Runtime.Data.ProtoLens.Service.Types
       as Data.ProtoLens.Service.Types
import qualified Data.ProtoLens.Runtime.Lens.Family2
       as Lens.Family2
import qualified Data.ProtoLens.Runtime.Lens.Family2.Unchecked
       as Lens.Family2.Unchecked
import qualified Data.ProtoLens.Runtime.Data.Text as Data.Text
import qualified Data.ProtoLens.Runtime.Data.Map as Data.Map
import qualified Data.ProtoLens.Runtime.Data.ByteString
       as Data.ByteString
import qualified Data.ProtoLens.Runtime.Data.ByteString.Char8
       as Data.ByteString.Char8
import qualified Data.ProtoLens.Runtime.Data.Text.Encoding
       as Data.Text.Encoding
import qualified Data.ProtoLens.Runtime.Data.Vector as Data.Vector
import qualified Data.ProtoLens.Runtime.Data.Vector.Generic
       as Data.Vector.Generic
import qualified Data.ProtoLens.Runtime.Data.Vector.Unboxed
       as Data.Vector.Unboxed
import qualified Data.ProtoLens.Runtime.Text.Read as Text.Read
import qualified Proto.Google.Api.Annotations
import qualified Proto.Google.Protobuf.Timestamp

accessToken ::
            forall f s a .
              (Prelude.Functor f,
               Data.ProtoLens.Field.HasField s "accessToken" a) =>
              Lens.Family2.LensLike' f s a
accessToken = Data.ProtoLens.Field.field @"accessToken"
auth ::
     forall f s a .
       (Prelude.Functor f, Data.ProtoLens.Field.HasField s "auth" a) =>
       Lens.Family2.LensLike' f s a
auth = Data.ProtoLens.Field.field @"auth"
baggage ::
        forall f s a .
          (Prelude.Functor f, Data.ProtoLens.Field.HasField s "baggage" a) =>
          Lens.Family2.LensLike' f s a
baggage = Data.ProtoLens.Field.field @"baggage"
boolValue ::
          forall f s a .
            (Prelude.Functor f,
             Data.ProtoLens.Field.HasField s "boolValue" a) =>
            Lens.Family2.LensLike' f s a
boolValue = Data.ProtoLens.Field.field @"boolValue"
commands ::
         forall f s a .
           (Prelude.Functor f,
            Data.ProtoLens.Field.HasField s "commands" a) =>
           Lens.Family2.LensLike' f s a
commands = Data.ProtoLens.Field.field @"commands"
counts ::
       forall f s a .
         (Prelude.Functor f, Data.ProtoLens.Field.HasField s "counts" a) =>
         Lens.Family2.LensLike' f s a
counts = Data.ProtoLens.Field.field @"counts"
devMode ::
        forall f s a .
          (Prelude.Functor f, Data.ProtoLens.Field.HasField s "devMode" a) =>
          Lens.Family2.LensLike' f s a
devMode = Data.ProtoLens.Field.field @"devMode"
disable ::
        forall f s a .
          (Prelude.Functor f, Data.ProtoLens.Field.HasField s "disable" a) =>
          Lens.Family2.LensLike' f s a
disable = Data.ProtoLens.Field.field @"disable"
doubleValue ::
            forall f s a .
              (Prelude.Functor f,
               Data.ProtoLens.Field.HasField s "doubleValue" a) =>
              Lens.Family2.LensLike' f s a
doubleValue = Data.ProtoLens.Field.field @"doubleValue"
durationMicros ::
               forall f s a .
                 (Prelude.Functor f,
                  Data.ProtoLens.Field.HasField s "durationMicros" a) =>
                 Lens.Family2.LensLike' f s a
durationMicros = Data.ProtoLens.Field.field @"durationMicros"
errors ::
       forall f s a .
         (Prelude.Functor f, Data.ProtoLens.Field.HasField s "errors" a) =>
         Lens.Family2.LensLike' f s a
errors = Data.ProtoLens.Field.field @"errors"
fields ::
       forall f s a .
         (Prelude.Functor f, Data.ProtoLens.Field.HasField s "fields" a) =>
         Lens.Family2.LensLike' f s a
fields = Data.ProtoLens.Field.field @"fields"
gauges ::
       forall f s a .
         (Prelude.Functor f, Data.ProtoLens.Field.HasField s "gauges" a) =>
         Lens.Family2.LensLike' f s a
gauges = Data.ProtoLens.Field.field @"gauges"
infos ::
      forall f s a .
        (Prelude.Functor f, Data.ProtoLens.Field.HasField s "infos" a) =>
        Lens.Family2.LensLike' f s a
infos = Data.ProtoLens.Field.field @"infos"
intValue ::
         forall f s a .
           (Prelude.Functor f,
            Data.ProtoLens.Field.HasField s "intValue" a) =>
           Lens.Family2.LensLike' f s a
intValue = Data.ProtoLens.Field.field @"intValue"
internalMetrics ::
                forall f s a .
                  (Prelude.Functor f,
                   Data.ProtoLens.Field.HasField s "internalMetrics" a) =>
                  Lens.Family2.LensLike' f s a
internalMetrics = Data.ProtoLens.Field.field @"internalMetrics"
jsonValue ::
          forall f s a .
            (Prelude.Functor f,
             Data.ProtoLens.Field.HasField s "jsonValue" a) =>
            Lens.Family2.LensLike' f s a
jsonValue = Data.ProtoLens.Field.field @"jsonValue"
key ::
    forall f s a .
      (Prelude.Functor f, Data.ProtoLens.Field.HasField s "key" a) =>
      Lens.Family2.LensLike' f s a
key = Data.ProtoLens.Field.field @"key"
logs ::
     forall f s a .
       (Prelude.Functor f, Data.ProtoLens.Field.HasField s "logs" a) =>
       Lens.Family2.LensLike' f s a
logs = Data.ProtoLens.Field.field @"logs"
maybe'auth ::
           forall f s a .
             (Prelude.Functor f,
              Data.ProtoLens.Field.HasField s "maybe'auth" a) =>
             Lens.Family2.LensLike' f s a
maybe'auth = Data.ProtoLens.Field.field @"maybe'auth"
maybe'boolValue ::
                forall f s a .
                  (Prelude.Functor f,
                   Data.ProtoLens.Field.HasField s "maybe'boolValue" a) =>
                  Lens.Family2.LensLike' f s a
maybe'boolValue = Data.ProtoLens.Field.field @"maybe'boolValue"
maybe'doubleValue ::
                  forall f s a .
                    (Prelude.Functor f,
                     Data.ProtoLens.Field.HasField s "maybe'doubleValue" a) =>
                    Lens.Family2.LensLike' f s a
maybe'doubleValue = Data.ProtoLens.Field.field @"maybe'doubleValue"
maybe'intValue ::
               forall f s a .
                 (Prelude.Functor f,
                  Data.ProtoLens.Field.HasField s "maybe'intValue" a) =>
                 Lens.Family2.LensLike' f s a
maybe'intValue = Data.ProtoLens.Field.field @"maybe'intValue"
maybe'internalMetrics ::
                      forall f s a .
                        (Prelude.Functor f,
                         Data.ProtoLens.Field.HasField s "maybe'internalMetrics" a) =>
                        Lens.Family2.LensLike' f s a
maybe'internalMetrics
  = Data.ProtoLens.Field.field @"maybe'internalMetrics"
maybe'jsonValue ::
                forall f s a .
                  (Prelude.Functor f,
                   Data.ProtoLens.Field.HasField s "maybe'jsonValue" a) =>
                  Lens.Family2.LensLike' f s a
maybe'jsonValue = Data.ProtoLens.Field.field @"maybe'jsonValue"
maybe'receiveTimestamp ::
                       forall f s a .
                         (Prelude.Functor f,
                          Data.ProtoLens.Field.HasField s "maybe'receiveTimestamp" a) =>
                         Lens.Family2.LensLike' f s a
maybe'receiveTimestamp
  = Data.ProtoLens.Field.field @"maybe'receiveTimestamp"
maybe'reporter ::
               forall f s a .
                 (Prelude.Functor f,
                  Data.ProtoLens.Field.HasField s "maybe'reporter" a) =>
                 Lens.Family2.LensLike' f s a
maybe'reporter = Data.ProtoLens.Field.field @"maybe'reporter"
maybe'spanContext ::
                  forall f s a .
                    (Prelude.Functor f,
                     Data.ProtoLens.Field.HasField s "maybe'spanContext" a) =>
                    Lens.Family2.LensLike' f s a
maybe'spanContext = Data.ProtoLens.Field.field @"maybe'spanContext"
maybe'startTimestamp ::
                     forall f s a .
                       (Prelude.Functor f,
                        Data.ProtoLens.Field.HasField s "maybe'startTimestamp" a) =>
                       Lens.Family2.LensLike' f s a
maybe'startTimestamp
  = Data.ProtoLens.Field.field @"maybe'startTimestamp"
maybe'stringValue ::
                  forall f s a .
                    (Prelude.Functor f,
                     Data.ProtoLens.Field.HasField s "maybe'stringValue" a) =>
                    Lens.Family2.LensLike' f s a
maybe'stringValue = Data.ProtoLens.Field.field @"maybe'stringValue"
maybe'timestamp ::
                forall f s a .
                  (Prelude.Functor f,
                   Data.ProtoLens.Field.HasField s "maybe'timestamp" a) =>
                  Lens.Family2.LensLike' f s a
maybe'timestamp = Data.ProtoLens.Field.field @"maybe'timestamp"
maybe'transmitTimestamp ::
                        forall f s a .
                          (Prelude.Functor f,
                           Data.ProtoLens.Field.HasField s "maybe'transmitTimestamp" a) =>
                          Lens.Family2.LensLike' f s a
maybe'transmitTimestamp
  = Data.ProtoLens.Field.field @"maybe'transmitTimestamp"
maybe'value ::
            forall f s a .
              (Prelude.Functor f,
               Data.ProtoLens.Field.HasField s "maybe'value" a) =>
              Lens.Family2.LensLike' f s a
maybe'value = Data.ProtoLens.Field.field @"maybe'value"
name ::
     forall f s a .
       (Prelude.Functor f, Data.ProtoLens.Field.HasField s "name" a) =>
       Lens.Family2.LensLike' f s a
name = Data.ProtoLens.Field.field @"name"
operationName ::
              forall f s a .
                (Prelude.Functor f,
                 Data.ProtoLens.Field.HasField s "operationName" a) =>
                Lens.Family2.LensLike' f s a
operationName = Data.ProtoLens.Field.field @"operationName"
receiveTimestamp ::
                 forall f s a .
                   (Prelude.Functor f,
                    Data.ProtoLens.Field.HasField s "receiveTimestamp" a) =>
                   Lens.Family2.LensLike' f s a
receiveTimestamp = Data.ProtoLens.Field.field @"receiveTimestamp"
references ::
           forall f s a .
             (Prelude.Functor f,
              Data.ProtoLens.Field.HasField s "references" a) =>
             Lens.Family2.LensLike' f s a
references = Data.ProtoLens.Field.field @"references"
relationship ::
             forall f s a .
               (Prelude.Functor f,
                Data.ProtoLens.Field.HasField s "relationship" a) =>
               Lens.Family2.LensLike' f s a
relationship = Data.ProtoLens.Field.field @"relationship"
reporter ::
         forall f s a .
           (Prelude.Functor f,
            Data.ProtoLens.Field.HasField s "reporter" a) =>
           Lens.Family2.LensLike' f s a
reporter = Data.ProtoLens.Field.field @"reporter"
reporterId ::
           forall f s a .
             (Prelude.Functor f,
              Data.ProtoLens.Field.HasField s "reporterId" a) =>
             Lens.Family2.LensLike' f s a
reporterId = Data.ProtoLens.Field.field @"reporterId"
spanContext ::
            forall f s a .
              (Prelude.Functor f,
               Data.ProtoLens.Field.HasField s "spanContext" a) =>
              Lens.Family2.LensLike' f s a
spanContext = Data.ProtoLens.Field.field @"spanContext"
spanId ::
       forall f s a .
         (Prelude.Functor f, Data.ProtoLens.Field.HasField s "spanId" a) =>
         Lens.Family2.LensLike' f s a
spanId = Data.ProtoLens.Field.field @"spanId"
spans ::
      forall f s a .
        (Prelude.Functor f, Data.ProtoLens.Field.HasField s "spans" a) =>
        Lens.Family2.LensLike' f s a
spans = Data.ProtoLens.Field.field @"spans"
startTimestamp ::
               forall f s a .
                 (Prelude.Functor f,
                  Data.ProtoLens.Field.HasField s "startTimestamp" a) =>
                 Lens.Family2.LensLike' f s a
startTimestamp = Data.ProtoLens.Field.field @"startTimestamp"
stringValue ::
            forall f s a .
              (Prelude.Functor f,
               Data.ProtoLens.Field.HasField s "stringValue" a) =>
              Lens.Family2.LensLike' f s a
stringValue = Data.ProtoLens.Field.field @"stringValue"
tags ::
     forall f s a .
       (Prelude.Functor f, Data.ProtoLens.Field.HasField s "tags" a) =>
       Lens.Family2.LensLike' f s a
tags = Data.ProtoLens.Field.field @"tags"
timestamp ::
          forall f s a .
            (Prelude.Functor f,
             Data.ProtoLens.Field.HasField s "timestamp" a) =>
            Lens.Family2.LensLike' f s a
timestamp = Data.ProtoLens.Field.field @"timestamp"
timestampOffsetMicros ::
                      forall f s a .
                        (Prelude.Functor f,
                         Data.ProtoLens.Field.HasField s "timestampOffsetMicros" a) =>
                        Lens.Family2.LensLike' f s a
timestampOffsetMicros
  = Data.ProtoLens.Field.field @"timestampOffsetMicros"
traceId ::
        forall f s a .
          (Prelude.Functor f, Data.ProtoLens.Field.HasField s "traceId" a) =>
          Lens.Family2.LensLike' f s a
traceId = Data.ProtoLens.Field.field @"traceId"
transmitTimestamp ::
                  forall f s a .
                    (Prelude.Functor f,
                     Data.ProtoLens.Field.HasField s "transmitTimestamp" a) =>
                    Lens.Family2.LensLike' f s a
transmitTimestamp = Data.ProtoLens.Field.field @"transmitTimestamp"
value ::
      forall f s a .
        (Prelude.Functor f, Data.ProtoLens.Field.HasField s "value" a) =>
        Lens.Family2.LensLike' f s a
value = Data.ProtoLens.Field.field @"value"
vec'commands ::
             forall f s a .
               (Prelude.Functor f,
                Data.ProtoLens.Field.HasField s "vec'commands" a) =>
               Lens.Family2.LensLike' f s a
vec'commands = Data.ProtoLens.Field.field @"vec'commands"
vec'counts ::
           forall f s a .
             (Prelude.Functor f,
              Data.ProtoLens.Field.HasField s "vec'counts" a) =>
             Lens.Family2.LensLike' f s a
vec'counts = Data.ProtoLens.Field.field @"vec'counts"
vec'errors ::
           forall f s a .
             (Prelude.Functor f,
              Data.ProtoLens.Field.HasField s "vec'errors" a) =>
             Lens.Family2.LensLike' f s a
vec'errors = Data.ProtoLens.Field.field @"vec'errors"
vec'fields ::
           forall f s a .
             (Prelude.Functor f,
              Data.ProtoLens.Field.HasField s "vec'fields" a) =>
             Lens.Family2.LensLike' f s a
vec'fields = Data.ProtoLens.Field.field @"vec'fields"
vec'gauges ::
           forall f s a .
             (Prelude.Functor f,
              Data.ProtoLens.Field.HasField s "vec'gauges" a) =>
             Lens.Family2.LensLike' f s a
vec'gauges = Data.ProtoLens.Field.field @"vec'gauges"
vec'infos ::
          forall f s a .
            (Prelude.Functor f,
             Data.ProtoLens.Field.HasField s "vec'infos" a) =>
            Lens.Family2.LensLike' f s a
vec'infos = Data.ProtoLens.Field.field @"vec'infos"
vec'logs ::
         forall f s a .
           (Prelude.Functor f,
            Data.ProtoLens.Field.HasField s "vec'logs" a) =>
           Lens.Family2.LensLike' f s a
vec'logs = Data.ProtoLens.Field.field @"vec'logs"
vec'references ::
               forall f s a .
                 (Prelude.Functor f,
                  Data.ProtoLens.Field.HasField s "vec'references" a) =>
                 Lens.Family2.LensLike' f s a
vec'references = Data.ProtoLens.Field.field @"vec'references"
vec'spans ::
          forall f s a .
            (Prelude.Functor f,
             Data.ProtoLens.Field.HasField s "vec'spans" a) =>
            Lens.Family2.LensLike' f s a
vec'spans = Data.ProtoLens.Field.field @"vec'spans"
vec'tags ::
         forall f s a .
           (Prelude.Functor f,
            Data.ProtoLens.Field.HasField s "vec'tags" a) =>
           Lens.Family2.LensLike' f s a
vec'tags = Data.ProtoLens.Field.field @"vec'tags"
vec'warnings ::
             forall f s a .
               (Prelude.Functor f,
                Data.ProtoLens.Field.HasField s "vec'warnings" a) =>
               Lens.Family2.LensLike' f s a
vec'warnings = Data.ProtoLens.Field.field @"vec'warnings"
warnings ::
         forall f s a .
           (Prelude.Functor f,
            Data.ProtoLens.Field.HasField s "warnings" a) =>
           Lens.Family2.LensLike' f s a
warnings = Data.ProtoLens.Field.field @"warnings"