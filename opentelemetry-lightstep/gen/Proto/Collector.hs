{- This file was auto-generated from collector.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies,
  UndecidableInstances, GeneralizedNewtypeDeriving,
  MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
  PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds,
  BangPatterns, TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports#-}
module Proto.Collector
       (CollectorService(..), Auth(), Command(), InternalMetrics(),
        KeyValue(), KeyValue'Value(..), _KeyValue'StringValue,
        _KeyValue'IntValue, _KeyValue'DoubleValue, _KeyValue'BoolValue,
        _KeyValue'JsonValue, Log(), MetricsSample(),
        MetricsSample'Value(..), _MetricsSample'IntValue,
        _MetricsSample'DoubleValue, Reference(),
        Reference'Relationship(..), Reference'Relationship(),
        Reference'Relationship'UnrecognizedValue, ReportRequest(),
        ReportResponse(), Reporter(), Span(), SpanContext(),
        SpanContext'BaggageEntry())
       where
import qualified Data.ProtoLens.Runtime.Control.DeepSeq
       as Control.DeepSeq
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Prism
       as Data.ProtoLens.Prism
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

{- | Fields :

    * 'Proto.Collector_Fields.accessToken' @:: Lens' Auth Data.Text.Text@
 -}
data Auth = Auth{_Auth'accessToken :: !Data.Text.Text,
                 _Auth'_unknownFields :: !Data.ProtoLens.FieldSet}
              deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Auth where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Auth "accessToken"
           (Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _Auth'accessToken
               (\ x__ y__ -> x__{_Auth'accessToken = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Message Auth where
        messageName _ = Data.Text.pack "lightstep.collector.Auth"
        fieldsByTag
          = let accessToken__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "access_token"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"accessToken"))
                      :: Data.ProtoLens.FieldDescriptor Auth
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, accessToken__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _Auth'_unknownFields
              (\ x__ y__ -> x__{_Auth'_unknownFields = y__})
        defMessage
          = Auth{_Auth'accessToken = Data.ProtoLens.fieldDefault,
                 _Auth'_unknownFields = ([])}
        parseMessage
          = let loop :: Auth -> Data.ProtoLens.Encoding.Bytes.Parser Auth
                loop x
                  = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
                       if end then
                         do let missing = [] in
                              if Prelude.null missing then Prelude.return () else
                                Prelude.fail
                                  (("Missing required fields: ") Prelude.++
                                     Prelude.show (missing :: ([Prelude.String])))
                            Prelude.return
                              (Lens.Family2.over Data.ProtoLens.unknownFields
                                 (\ !t -> Prelude.reverse t)
                                 x)
                         else
                         do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                            case tag of
                                10 -> do y <- (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                              Data.ProtoLens.Encoding.Bytes.getBytes
                                                                (Prelude.fromIntegral len)
                                                  Data.ProtoLens.Encoding.Bytes.runEither
                                                    (case Data.Text.Encoding.decodeUtf8' value of
                                                         Prelude.Left err -> Prelude.Left
                                                                               (Prelude.show err)
                                                         Prelude.Right r -> Prelude.Right r))
                                                Data.ProtoLens.Encoding.Bytes.<?> "access_token"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"accessToken")
                                              y
                                              x)
                                wire -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                                   wire
                                           loop
                                             (Lens.Family2.over Data.ProtoLens.unknownFields
                                                (\ !t -> (:) y t)
                                                x)
              in
              (do loop Data.ProtoLens.defMessage)
                Data.ProtoLens.Encoding.Bytes.<?> "Auth"
        buildMessage
          = (\ _x ->
               (let _v
                      = Lens.Family2.view (Data.ProtoLens.Field.field @"accessToken") _x
                  in
                  if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty else
                    (Data.ProtoLens.Encoding.Bytes.putVarInt 10) Data.Monoid.<>
                      (((\ bs ->
                           (Data.ProtoLens.Encoding.Bytes.putVarInt
                              (Prelude.fromIntegral (Data.ByteString.length bs)))
                             Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Prelude.. Data.Text.Encoding.encodeUtf8)
                        _v)
                 Data.Monoid.<>
                 Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData Auth where
        rnf
          = (\ x__ ->
               Control.DeepSeq.deepseq (_Auth'_unknownFields x__)
                 (Control.DeepSeq.deepseq (_Auth'accessToken x__) (())))
{- | Fields :

    * 'Proto.Collector_Fields.disable' @:: Lens' Command Prelude.Bool@
    * 'Proto.Collector_Fields.devMode' @:: Lens' Command Prelude.Bool@
 -}
data Command = Command{_Command'disable :: !Prelude.Bool,
                       _Command'devMode :: !Prelude.Bool,
                       _Command'_unknownFields :: !Data.ProtoLens.FieldSet}
                 deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Command where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Command "disable"
           (Prelude.Bool)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _Command'disable
               (\ x__ y__ -> x__{_Command'disable = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField Command "devMode"
           (Prelude.Bool)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _Command'devMode
               (\ x__ y__ -> x__{_Command'devMode = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Message Command where
        messageName _ = Data.Text.pack "lightstep.collector.Command"
        fieldsByTag
          = let disable__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "disable"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"disable"))
                      :: Data.ProtoLens.FieldDescriptor Command
                devMode__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "dev_mode"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"devMode"))
                      :: Data.ProtoLens.FieldDescriptor Command
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, disable__field_descriptor),
                 (Data.ProtoLens.Tag 2, devMode__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _Command'_unknownFields
              (\ x__ y__ -> x__{_Command'_unknownFields = y__})
        defMessage
          = Command{_Command'disable = Data.ProtoLens.fieldDefault,
                    _Command'devMode = Data.ProtoLens.fieldDefault,
                    _Command'_unknownFields = ([])}
        parseMessage
          = let loop ::
                     Command -> Data.ProtoLens.Encoding.Bytes.Parser Command
                loop x
                  = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
                       if end then
                         do let missing = [] in
                              if Prelude.null missing then Prelude.return () else
                                Prelude.fail
                                  (("Missing required fields: ") Prelude.++
                                     Prelude.show (missing :: ([Prelude.String])))
                            Prelude.return
                              (Lens.Family2.over Data.ProtoLens.unknownFields
                                 (\ !t -> Prelude.reverse t)
                                 x)
                         else
                         do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                            case tag of
                                8 -> do y <- (Prelude.fmap ((Prelude./=) 0)
                                                Data.ProtoLens.Encoding.Bytes.getVarInt)
                                               Data.ProtoLens.Encoding.Bytes.<?> "disable"
                                        loop
                                          (Lens.Family2.set (Data.ProtoLens.Field.field @"disable")
                                             y
                                             x)
                                16 -> do y <- (Prelude.fmap ((Prelude./=) 0)
                                                 Data.ProtoLens.Encoding.Bytes.getVarInt)
                                                Data.ProtoLens.Encoding.Bytes.<?> "dev_mode"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"devMode")
                                              y
                                              x)
                                wire -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                                   wire
                                           loop
                                             (Lens.Family2.over Data.ProtoLens.unknownFields
                                                (\ !t -> (:) y t)
                                                x)
              in
              (do loop Data.ProtoLens.defMessage)
                Data.ProtoLens.Encoding.Bytes.<?> "Command"
        buildMessage
          = (\ _x ->
               (let _v
                      = Lens.Family2.view (Data.ProtoLens.Field.field @"disable") _x
                  in
                  if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty else
                    (Data.ProtoLens.Encoding.Bytes.putVarInt 8) Data.Monoid.<>
                      ((Data.ProtoLens.Encoding.Bytes.putVarInt) Prelude..
                         (\ b -> if b then 1 else 0))
                        _v)
                 Data.Monoid.<>
                 (let _v
                        = Lens.Family2.view (Data.ProtoLens.Field.field @"devMode") _x
                    in
                    if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                      Data.Monoid.mempty else
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 16) Data.Monoid.<>
                        ((Data.ProtoLens.Encoding.Bytes.putVarInt) Prelude..
                           (\ b -> if b then 1 else 0))
                          _v)
                   Data.Monoid.<>
                   Data.ProtoLens.Encoding.Wire.buildFieldSet
                     (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData Command where
        rnf
          = (\ x__ ->
               Control.DeepSeq.deepseq (_Command'_unknownFields x__)
                 (Control.DeepSeq.deepseq (_Command'disable x__)
                    (Control.DeepSeq.deepseq (_Command'devMode x__) (()))))
{- | Fields :

    * 'Proto.Collector_Fields.startTimestamp' @:: Lens' InternalMetrics Proto.Google.Protobuf.Timestamp.Timestamp@
    * 'Proto.Collector_Fields.maybe'startTimestamp' @:: Lens' InternalMetrics
  (Prelude.Maybe Proto.Google.Protobuf.Timestamp.Timestamp)@
    * 'Proto.Collector_Fields.durationMicros' @:: Lens' InternalMetrics Data.Word.Word64@
    * 'Proto.Collector_Fields.logs' @:: Lens' InternalMetrics [Log]@
    * 'Proto.Collector_Fields.vec'logs' @:: Lens' InternalMetrics (Data.Vector.Vector Log)@
    * 'Proto.Collector_Fields.counts' @:: Lens' InternalMetrics [MetricsSample]@
    * 'Proto.Collector_Fields.vec'counts' @:: Lens' InternalMetrics (Data.Vector.Vector MetricsSample)@
    * 'Proto.Collector_Fields.gauges' @:: Lens' InternalMetrics [MetricsSample]@
    * 'Proto.Collector_Fields.vec'gauges' @:: Lens' InternalMetrics (Data.Vector.Vector MetricsSample)@
 -}
data InternalMetrics = InternalMetrics{_InternalMetrics'startTimestamp
                                       ::
                                       !(Prelude.Maybe Proto.Google.Protobuf.Timestamp.Timestamp),
                                       _InternalMetrics'durationMicros :: !Data.Word.Word64,
                                       _InternalMetrics'logs :: !(Data.Vector.Vector Log),
                                       _InternalMetrics'counts ::
                                       !(Data.Vector.Vector MetricsSample),
                                       _InternalMetrics'gauges ::
                                       !(Data.Vector.Vector MetricsSample),
                                       _InternalMetrics'_unknownFields :: !Data.ProtoLens.FieldSet}
                         deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show InternalMetrics where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField InternalMetrics
           "startTimestamp"
           (Proto.Google.Protobuf.Timestamp.Timestamp)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _InternalMetrics'startTimestamp
               (\ x__ y__ -> x__{_InternalMetrics'startTimestamp = y__}))
              Prelude.. Data.ProtoLens.maybeLens Data.ProtoLens.defMessage
instance Data.ProtoLens.Field.HasField InternalMetrics
           "maybe'startTimestamp"
           (Prelude.Maybe Proto.Google.Protobuf.Timestamp.Timestamp)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _InternalMetrics'startTimestamp
               (\ x__ y__ -> x__{_InternalMetrics'startTimestamp = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField InternalMetrics
           "durationMicros"
           (Data.Word.Word64)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _InternalMetrics'durationMicros
               (\ x__ y__ -> x__{_InternalMetrics'durationMicros = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField InternalMetrics "logs"
           ([Log])
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _InternalMetrics'logs
               (\ x__ y__ -> x__{_InternalMetrics'logs = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens Data.Vector.Generic.toList
                (\ _ y__ -> Data.Vector.Generic.fromList y__)
instance Data.ProtoLens.Field.HasField InternalMetrics "vec'logs"
           (Data.Vector.Vector Log)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _InternalMetrics'logs
               (\ x__ y__ -> x__{_InternalMetrics'logs = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField InternalMetrics "counts"
           ([MetricsSample])
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _InternalMetrics'counts
               (\ x__ y__ -> x__{_InternalMetrics'counts = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens Data.Vector.Generic.toList
                (\ _ y__ -> Data.Vector.Generic.fromList y__)
instance Data.ProtoLens.Field.HasField InternalMetrics "vec'counts"
           (Data.Vector.Vector MetricsSample)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _InternalMetrics'counts
               (\ x__ y__ -> x__{_InternalMetrics'counts = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField InternalMetrics "gauges"
           ([MetricsSample])
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _InternalMetrics'gauges
               (\ x__ y__ -> x__{_InternalMetrics'gauges = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens Data.Vector.Generic.toList
                (\ _ y__ -> Data.Vector.Generic.fromList y__)
instance Data.ProtoLens.Field.HasField InternalMetrics "vec'gauges"
           (Data.Vector.Vector MetricsSample)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _InternalMetrics'gauges
               (\ x__ y__ -> x__{_InternalMetrics'gauges = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Message InternalMetrics where
        messageName _
          = Data.Text.pack "lightstep.collector.InternalMetrics"
        fieldsByTag
          = let startTimestamp__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "start_timestamp"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor
                           Proto.Google.Protobuf.Timestamp.Timestamp)
                      (Data.ProtoLens.OptionalField
                         (Data.ProtoLens.Field.field @"maybe'startTimestamp"))
                      :: Data.ProtoLens.FieldDescriptor InternalMetrics
                durationMicros__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "duration_micros"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"durationMicros"))
                      :: Data.ProtoLens.FieldDescriptor InternalMetrics
                logs__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "logs"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor Log)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Data.ProtoLens.Field.field @"logs"))
                      :: Data.ProtoLens.FieldDescriptor InternalMetrics
                counts__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "counts"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor MetricsSample)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Data.ProtoLens.Field.field @"counts"))
                      :: Data.ProtoLens.FieldDescriptor InternalMetrics
                gauges__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "gauges"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor MetricsSample)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Data.ProtoLens.Field.field @"gauges"))
                      :: Data.ProtoLens.FieldDescriptor InternalMetrics
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, startTimestamp__field_descriptor),
                 (Data.ProtoLens.Tag 2, durationMicros__field_descriptor),
                 (Data.ProtoLens.Tag 3, logs__field_descriptor),
                 (Data.ProtoLens.Tag 4, counts__field_descriptor),
                 (Data.ProtoLens.Tag 5, gauges__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _InternalMetrics'_unknownFields
              (\ x__ y__ -> x__{_InternalMetrics'_unknownFields = y__})
        defMessage
          = InternalMetrics{_InternalMetrics'startTimestamp =
                              Prelude.Nothing,
                            _InternalMetrics'durationMicros = Data.ProtoLens.fieldDefault,
                            _InternalMetrics'logs = Data.Vector.Generic.empty,
                            _InternalMetrics'counts = Data.Vector.Generic.empty,
                            _InternalMetrics'gauges = Data.Vector.Generic.empty,
                            _InternalMetrics'_unknownFields = ([])}
        parseMessage
          = let loop ::
                     InternalMetrics ->
                       Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector
                         Data.ProtoLens.Encoding.Growing.RealWorld
                         MetricsSample
                         ->
                         Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector
                           Data.ProtoLens.Encoding.Growing.RealWorld
                           MetricsSample
                           ->
                           Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector
                             Data.ProtoLens.Encoding.Growing.RealWorld
                             Log
                             -> Data.ProtoLens.Encoding.Bytes.Parser InternalMetrics
                loop x mutable'counts mutable'gauges mutable'logs
                  = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
                       if end then
                         do frozen'counts <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                               (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                  mutable'counts)
                            frozen'gauges <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                               (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                  mutable'gauges)
                            frozen'logs <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                             (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                mutable'logs)
                            let missing = [] in
                              if Prelude.null missing then Prelude.return () else
                                Prelude.fail
                                  (("Missing required fields: ") Prelude.++
                                     Prelude.show (missing :: ([Prelude.String])))
                            Prelude.return
                              (Lens.Family2.over Data.ProtoLens.unknownFields
                                 (\ !t -> Prelude.reverse t)
                                 (Lens.Family2.set (Data.ProtoLens.Field.field @"vec'counts")
                                    frozen'counts
                                    (Lens.Family2.set (Data.ProtoLens.Field.field @"vec'gauges")
                                       frozen'gauges
                                       (Lens.Family2.set (Data.ProtoLens.Field.field @"vec'logs")
                                          frozen'logs
                                          x))))
                         else
                         do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                            case tag of
                                10 -> do y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                  Data.ProtoLens.Encoding.Bytes.isolate
                                                    (Prelude.fromIntegral len)
                                                    Data.ProtoLens.parseMessage)
                                                Data.ProtoLens.Encoding.Bytes.<?> "start_timestamp"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"startTimestamp")
                                              y
                                              x)
                                           mutable'counts
                                           mutable'gauges
                                           mutable'logs
                                16 -> do y <- (Data.ProtoLens.Encoding.Bytes.getVarInt)
                                                Data.ProtoLens.Encoding.Bytes.<?> "duration_micros"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"durationMicros")
                                              y
                                              x)
                                           mutable'counts
                                           mutable'gauges
                                           mutable'logs
                                26 -> do !y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                   Data.ProtoLens.Encoding.Bytes.isolate
                                                     (Prelude.fromIntegral len)
                                                     Data.ProtoLens.parseMessage)
                                                 Data.ProtoLens.Encoding.Bytes.<?> "logs"
                                         v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                (Data.ProtoLens.Encoding.Growing.append mutable'logs
                                                   y)
                                         loop x mutable'counts mutable'gauges v
                                34 -> do !y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                   Data.ProtoLens.Encoding.Bytes.isolate
                                                     (Prelude.fromIntegral len)
                                                     Data.ProtoLens.parseMessage)
                                                 Data.ProtoLens.Encoding.Bytes.<?> "counts"
                                         v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                (Data.ProtoLens.Encoding.Growing.append
                                                   mutable'counts
                                                   y)
                                         loop x v mutable'gauges mutable'logs
                                42 -> do !y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                   Data.ProtoLens.Encoding.Bytes.isolate
                                                     (Prelude.fromIntegral len)
                                                     Data.ProtoLens.parseMessage)
                                                 Data.ProtoLens.Encoding.Bytes.<?> "gauges"
                                         v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                (Data.ProtoLens.Encoding.Growing.append
                                                   mutable'gauges
                                                   y)
                                         loop x mutable'counts v mutable'logs
                                wire -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                                   wire
                                           loop
                                             (Lens.Family2.over Data.ProtoLens.unknownFields
                                                (\ !t -> (:) y t)
                                                x)
                                             mutable'counts
                                             mutable'gauges
                                             mutable'logs
              in
              (do mutable'counts <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                      Data.ProtoLens.Encoding.Growing.new
                  mutable'gauges <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                      Data.ProtoLens.Encoding.Growing.new
                  mutable'logs <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                    Data.ProtoLens.Encoding.Growing.new
                  loop Data.ProtoLens.defMessage mutable'counts mutable'gauges
                    mutable'logs)
                Data.ProtoLens.Encoding.Bytes.<?> "InternalMetrics"
        buildMessage
          = (\ _x ->
               (case
                  Lens.Family2.view
                    (Data.ProtoLens.Field.field @"maybe'startTimestamp")
                    _x
                  of
                    (Prelude.Nothing) -> Data.Monoid.mempty
                    Prelude.Just _v -> (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                                         Data.Monoid.<>
                                         (((\ bs ->
                                              (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                 (Prelude.fromIntegral (Data.ByteString.length bs)))
                                                Data.Monoid.<>
                                                Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                            Prelude.. Data.ProtoLens.encodeMessage)
                                           _v)
                 Data.Monoid.<>
                 (let _v
                        = Lens.Family2.view (Data.ProtoLens.Field.field @"durationMicros")
                            _x
                    in
                    if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                      Data.Monoid.mempty else
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 16) Data.Monoid.<>
                        Data.ProtoLens.Encoding.Bytes.putVarInt _v)
                   Data.Monoid.<>
                   (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                      (\ _v ->
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 26) Data.Monoid.<>
                           (((\ bs ->
                                (Data.ProtoLens.Encoding.Bytes.putVarInt
                                   (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                              Prelude.. Data.ProtoLens.encodeMessage)
                             _v)
                      (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'logs") _x))
                     Data.Monoid.<>
                     (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                        (\ _v ->
                           (Data.ProtoLens.Encoding.Bytes.putVarInt 34) Data.Monoid.<>
                             (((\ bs ->
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                    Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                Prelude.. Data.ProtoLens.encodeMessage)
                               _v)
                        (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'counts") _x))
                       Data.Monoid.<>
                       (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                          (\ _v ->
                             (Data.ProtoLens.Encoding.Bytes.putVarInt 42) Data.Monoid.<>
                               (((\ bs ->
                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                       (Prelude.fromIntegral (Data.ByteString.length bs)))
                                      Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                  Prelude.. Data.ProtoLens.encodeMessage)
                                 _v)
                          (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'gauges") _x))
                         Data.Monoid.<>
                         Data.ProtoLens.Encoding.Wire.buildFieldSet
                           (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData InternalMetrics where
        rnf
          = (\ x__ ->
               Control.DeepSeq.deepseq (_InternalMetrics'_unknownFields x__)
                 (Control.DeepSeq.deepseq (_InternalMetrics'startTimestamp x__)
                    (Control.DeepSeq.deepseq (_InternalMetrics'durationMicros x__)
                       (Control.DeepSeq.deepseq (_InternalMetrics'logs x__)
                          (Control.DeepSeq.deepseq (_InternalMetrics'counts x__)
                             (Control.DeepSeq.deepseq (_InternalMetrics'gauges x__) (())))))))
{- | Fields :

    * 'Proto.Collector_Fields.key' @:: Lens' KeyValue Data.Text.Text@
    * 'Proto.Collector_Fields.maybe'value' @:: Lens' KeyValue (Prelude.Maybe KeyValue'Value)@
    * 'Proto.Collector_Fields.maybe'stringValue' @:: Lens' KeyValue (Prelude.Maybe Data.Text.Text)@
    * 'Proto.Collector_Fields.stringValue' @:: Lens' KeyValue Data.Text.Text@
    * 'Proto.Collector_Fields.maybe'intValue' @:: Lens' KeyValue (Prelude.Maybe Data.Int.Int64)@
    * 'Proto.Collector_Fields.intValue' @:: Lens' KeyValue Data.Int.Int64@
    * 'Proto.Collector_Fields.maybe'doubleValue' @:: Lens' KeyValue (Prelude.Maybe Prelude.Double)@
    * 'Proto.Collector_Fields.doubleValue' @:: Lens' KeyValue Prelude.Double@
    * 'Proto.Collector_Fields.maybe'boolValue' @:: Lens' KeyValue (Prelude.Maybe Prelude.Bool)@
    * 'Proto.Collector_Fields.boolValue' @:: Lens' KeyValue Prelude.Bool@
    * 'Proto.Collector_Fields.maybe'jsonValue' @:: Lens' KeyValue (Prelude.Maybe Data.Text.Text)@
    * 'Proto.Collector_Fields.jsonValue' @:: Lens' KeyValue Data.Text.Text@
 -}
data KeyValue = KeyValue{_KeyValue'key :: !Data.Text.Text,
                         _KeyValue'value :: !(Prelude.Maybe KeyValue'Value),
                         _KeyValue'_unknownFields :: !Data.ProtoLens.FieldSet}
                  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show KeyValue where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
data KeyValue'Value = KeyValue'StringValue !Data.Text.Text
                    | KeyValue'IntValue !Data.Int.Int64
                    | KeyValue'DoubleValue !Prelude.Double
                    | KeyValue'BoolValue !Prelude.Bool
                    | KeyValue'JsonValue !Data.Text.Text
                        deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField KeyValue "key"
           (Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _KeyValue'key
               (\ x__ y__ -> x__{_KeyValue'key = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField KeyValue "maybe'value"
           (Prelude.Maybe KeyValue'Value)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _KeyValue'value
               (\ x__ y__ -> x__{_KeyValue'value = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField KeyValue "maybe'stringValue"
           (Prelude.Maybe Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _KeyValue'value
               (\ x__ y__ -> x__{_KeyValue'value = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens
                (\ x__ ->
                   case x__ of
                       Prelude.Just (KeyValue'StringValue x__val) -> Prelude.Just x__val
                       _otherwise -> Prelude.Nothing)
                (\ _ y__ -> Prelude.fmap KeyValue'StringValue y__)
instance Data.ProtoLens.Field.HasField KeyValue "stringValue"
           (Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _KeyValue'value
               (\ x__ y__ -> x__{_KeyValue'value = y__}))
              Prelude..
              (Lens.Family2.Unchecked.lens
                 (\ x__ ->
                    case x__ of
                        Prelude.Just (KeyValue'StringValue x__val) -> Prelude.Just x__val
                        _otherwise -> Prelude.Nothing)
                 (\ _ y__ -> Prelude.fmap KeyValue'StringValue y__))
                Prelude.. Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault
instance Data.ProtoLens.Field.HasField KeyValue "maybe'intValue"
           (Prelude.Maybe Data.Int.Int64)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _KeyValue'value
               (\ x__ y__ -> x__{_KeyValue'value = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens
                (\ x__ ->
                   case x__ of
                       Prelude.Just (KeyValue'IntValue x__val) -> Prelude.Just x__val
                       _otherwise -> Prelude.Nothing)
                (\ _ y__ -> Prelude.fmap KeyValue'IntValue y__)
instance Data.ProtoLens.Field.HasField KeyValue "intValue"
           (Data.Int.Int64)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _KeyValue'value
               (\ x__ y__ -> x__{_KeyValue'value = y__}))
              Prelude..
              (Lens.Family2.Unchecked.lens
                 (\ x__ ->
                    case x__ of
                        Prelude.Just (KeyValue'IntValue x__val) -> Prelude.Just x__val
                        _otherwise -> Prelude.Nothing)
                 (\ _ y__ -> Prelude.fmap KeyValue'IntValue y__))
                Prelude.. Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault
instance Data.ProtoLens.Field.HasField KeyValue "maybe'doubleValue"
           (Prelude.Maybe Prelude.Double)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _KeyValue'value
               (\ x__ y__ -> x__{_KeyValue'value = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens
                (\ x__ ->
                   case x__ of
                       Prelude.Just (KeyValue'DoubleValue x__val) -> Prelude.Just x__val
                       _otherwise -> Prelude.Nothing)
                (\ _ y__ -> Prelude.fmap KeyValue'DoubleValue y__)
instance Data.ProtoLens.Field.HasField KeyValue "doubleValue"
           (Prelude.Double)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _KeyValue'value
               (\ x__ y__ -> x__{_KeyValue'value = y__}))
              Prelude..
              (Lens.Family2.Unchecked.lens
                 (\ x__ ->
                    case x__ of
                        Prelude.Just (KeyValue'DoubleValue x__val) -> Prelude.Just x__val
                        _otherwise -> Prelude.Nothing)
                 (\ _ y__ -> Prelude.fmap KeyValue'DoubleValue y__))
                Prelude.. Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault
instance Data.ProtoLens.Field.HasField KeyValue "maybe'boolValue"
           (Prelude.Maybe Prelude.Bool)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _KeyValue'value
               (\ x__ y__ -> x__{_KeyValue'value = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens
                (\ x__ ->
                   case x__ of
                       Prelude.Just (KeyValue'BoolValue x__val) -> Prelude.Just x__val
                       _otherwise -> Prelude.Nothing)
                (\ _ y__ -> Prelude.fmap KeyValue'BoolValue y__)
instance Data.ProtoLens.Field.HasField KeyValue "boolValue"
           (Prelude.Bool)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _KeyValue'value
               (\ x__ y__ -> x__{_KeyValue'value = y__}))
              Prelude..
              (Lens.Family2.Unchecked.lens
                 (\ x__ ->
                    case x__ of
                        Prelude.Just (KeyValue'BoolValue x__val) -> Prelude.Just x__val
                        _otherwise -> Prelude.Nothing)
                 (\ _ y__ -> Prelude.fmap KeyValue'BoolValue y__))
                Prelude.. Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault
instance Data.ProtoLens.Field.HasField KeyValue "maybe'jsonValue"
           (Prelude.Maybe Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _KeyValue'value
               (\ x__ y__ -> x__{_KeyValue'value = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens
                (\ x__ ->
                   case x__ of
                       Prelude.Just (KeyValue'JsonValue x__val) -> Prelude.Just x__val
                       _otherwise -> Prelude.Nothing)
                (\ _ y__ -> Prelude.fmap KeyValue'JsonValue y__)
instance Data.ProtoLens.Field.HasField KeyValue "jsonValue"
           (Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _KeyValue'value
               (\ x__ y__ -> x__{_KeyValue'value = y__}))
              Prelude..
              (Lens.Family2.Unchecked.lens
                 (\ x__ ->
                    case x__ of
                        Prelude.Just (KeyValue'JsonValue x__val) -> Prelude.Just x__val
                        _otherwise -> Prelude.Nothing)
                 (\ _ y__ -> Prelude.fmap KeyValue'JsonValue y__))
                Prelude.. Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault
instance Data.ProtoLens.Message KeyValue where
        messageName _ = Data.Text.pack "lightstep.collector.KeyValue"
        fieldsByTag
          = let key__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "key"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"key"))
                      :: Data.ProtoLens.FieldDescriptor KeyValue
                stringValue__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "string_value"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.OptionalField
                         (Data.ProtoLens.Field.field @"maybe'stringValue"))
                      :: Data.ProtoLens.FieldDescriptor KeyValue
                intValue__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "int_value"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
                      (Data.ProtoLens.OptionalField
                         (Data.ProtoLens.Field.field @"maybe'intValue"))
                      :: Data.ProtoLens.FieldDescriptor KeyValue
                doubleValue__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "double_value"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.DoubleField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Double)
                      (Data.ProtoLens.OptionalField
                         (Data.ProtoLens.Field.field @"maybe'doubleValue"))
                      :: Data.ProtoLens.FieldDescriptor KeyValue
                boolValue__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "bool_value"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.OptionalField
                         (Data.ProtoLens.Field.field @"maybe'boolValue"))
                      :: Data.ProtoLens.FieldDescriptor KeyValue
                jsonValue__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "json_value"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.OptionalField
                         (Data.ProtoLens.Field.field @"maybe'jsonValue"))
                      :: Data.ProtoLens.FieldDescriptor KeyValue
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, key__field_descriptor),
                 (Data.ProtoLens.Tag 2, stringValue__field_descriptor),
                 (Data.ProtoLens.Tag 3, intValue__field_descriptor),
                 (Data.ProtoLens.Tag 4, doubleValue__field_descriptor),
                 (Data.ProtoLens.Tag 5, boolValue__field_descriptor),
                 (Data.ProtoLens.Tag 6, jsonValue__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _KeyValue'_unknownFields
              (\ x__ y__ -> x__{_KeyValue'_unknownFields = y__})
        defMessage
          = KeyValue{_KeyValue'key = Data.ProtoLens.fieldDefault,
                     _KeyValue'value = Prelude.Nothing, _KeyValue'_unknownFields = ([])}
        parseMessage
          = let loop ::
                     KeyValue -> Data.ProtoLens.Encoding.Bytes.Parser KeyValue
                loop x
                  = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
                       if end then
                         do let missing = [] in
                              if Prelude.null missing then Prelude.return () else
                                Prelude.fail
                                  (("Missing required fields: ") Prelude.++
                                     Prelude.show (missing :: ([Prelude.String])))
                            Prelude.return
                              (Lens.Family2.over Data.ProtoLens.unknownFields
                                 (\ !t -> Prelude.reverse t)
                                 x)
                         else
                         do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                            case tag of
                                10 -> do y <- (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                              Data.ProtoLens.Encoding.Bytes.getBytes
                                                                (Prelude.fromIntegral len)
                                                  Data.ProtoLens.Encoding.Bytes.runEither
                                                    (case Data.Text.Encoding.decodeUtf8' value of
                                                         Prelude.Left err -> Prelude.Left
                                                                               (Prelude.show err)
                                                         Prelude.Right r -> Prelude.Right r))
                                                Data.ProtoLens.Encoding.Bytes.<?> "key"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"key") y
                                              x)
                                18 -> do y <- (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                              Data.ProtoLens.Encoding.Bytes.getBytes
                                                                (Prelude.fromIntegral len)
                                                  Data.ProtoLens.Encoding.Bytes.runEither
                                                    (case Data.Text.Encoding.decodeUtf8' value of
                                                         Prelude.Left err -> Prelude.Left
                                                                               (Prelude.show err)
                                                         Prelude.Right r -> Prelude.Right r))
                                                Data.ProtoLens.Encoding.Bytes.<?> "string_value"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"stringValue")
                                              y
                                              x)
                                24 -> do y <- (Prelude.fmap Prelude.fromIntegral
                                                 Data.ProtoLens.Encoding.Bytes.getVarInt)
                                                Data.ProtoLens.Encoding.Bytes.<?> "int_value"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"intValue")
                                              y
                                              x)
                                33 -> do y <- (Prelude.fmap
                                                 Data.ProtoLens.Encoding.Bytes.wordToDouble
                                                 Data.ProtoLens.Encoding.Bytes.getFixed64)
                                                Data.ProtoLens.Encoding.Bytes.<?> "double_value"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"doubleValue")
                                              y
                                              x)
                                40 -> do y <- (Prelude.fmap ((Prelude./=) 0)
                                                 Data.ProtoLens.Encoding.Bytes.getVarInt)
                                                Data.ProtoLens.Encoding.Bytes.<?> "bool_value"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"boolValue")
                                              y
                                              x)
                                50 -> do y <- (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                              Data.ProtoLens.Encoding.Bytes.getBytes
                                                                (Prelude.fromIntegral len)
                                                  Data.ProtoLens.Encoding.Bytes.runEither
                                                    (case Data.Text.Encoding.decodeUtf8' value of
                                                         Prelude.Left err -> Prelude.Left
                                                                               (Prelude.show err)
                                                         Prelude.Right r -> Prelude.Right r))
                                                Data.ProtoLens.Encoding.Bytes.<?> "json_value"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"jsonValue")
                                              y
                                              x)
                                wire -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                                   wire
                                           loop
                                             (Lens.Family2.over Data.ProtoLens.unknownFields
                                                (\ !t -> (:) y t)
                                                x)
              in
              (do loop Data.ProtoLens.defMessage)
                Data.ProtoLens.Encoding.Bytes.<?> "KeyValue"
        buildMessage
          = (\ _x ->
               (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"key") _x
                  in
                  if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty else
                    (Data.ProtoLens.Encoding.Bytes.putVarInt 10) Data.Monoid.<>
                      (((\ bs ->
                           (Data.ProtoLens.Encoding.Bytes.putVarInt
                              (Prelude.fromIntegral (Data.ByteString.length bs)))
                             Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Prelude.. Data.Text.Encoding.encodeUtf8)
                        _v)
                 Data.Monoid.<>
                 (case
                    Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'value") _x of
                      (Prelude.Nothing) -> Data.Monoid.mempty
                      Prelude.Just
                        (KeyValue'StringValue
                           v) -> (Data.ProtoLens.Encoding.Bytes.putVarInt 18) Data.Monoid.<>
                                   (((\ bs ->
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                           (Prelude.fromIntegral (Data.ByteString.length bs)))
                                          Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                      Prelude.. Data.Text.Encoding.encodeUtf8)
                                     v
                      Prelude.Just
                        (KeyValue'IntValue v) -> (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                    24)
                                                   Data.Monoid.<>
                                                   ((Data.ProtoLens.Encoding.Bytes.putVarInt)
                                                      Prelude.. Prelude.fromIntegral)
                                                     v
                      Prelude.Just
                        (KeyValue'DoubleValue
                           v) -> (Data.ProtoLens.Encoding.Bytes.putVarInt 33) Data.Monoid.<>
                                   ((Data.ProtoLens.Encoding.Bytes.putFixed64) Prelude..
                                      Data.ProtoLens.Encoding.Bytes.doubleToWord)
                                     v
                      Prelude.Just
                        (KeyValue'BoolValue v) -> (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                     40)
                                                    Data.Monoid.<>
                                                    ((Data.ProtoLens.Encoding.Bytes.putVarInt)
                                                       Prelude.. (\ b -> if b then 1 else 0))
                                                      v
                      Prelude.Just
                        (KeyValue'JsonValue v) -> (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                     50)
                                                    Data.Monoid.<>
                                                    (((\ bs ->
                                                         (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                            (Prelude.fromIntegral
                                                               (Data.ByteString.length bs)))
                                                           Data.Monoid.<>
                                                           Data.ProtoLens.Encoding.Bytes.putBytes
                                                             bs))
                                                       Prelude.. Data.Text.Encoding.encodeUtf8)
                                                      v)
                   Data.Monoid.<>
                   Data.ProtoLens.Encoding.Wire.buildFieldSet
                     (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData KeyValue where
        rnf
          = (\ x__ ->
               Control.DeepSeq.deepseq (_KeyValue'_unknownFields x__)
                 (Control.DeepSeq.deepseq (_KeyValue'key x__)
                    (Control.DeepSeq.deepseq (_KeyValue'value x__) (()))))
instance Control.DeepSeq.NFData KeyValue'Value where
        rnf (KeyValue'StringValue x__) = Control.DeepSeq.rnf x__
        rnf (KeyValue'IntValue x__) = Control.DeepSeq.rnf x__
        rnf (KeyValue'DoubleValue x__) = Control.DeepSeq.rnf x__
        rnf (KeyValue'BoolValue x__) = Control.DeepSeq.rnf x__
        rnf (KeyValue'JsonValue x__) = Control.DeepSeq.rnf x__
_KeyValue'StringValue ::
                      Data.ProtoLens.Prism.Prism' KeyValue'Value Data.Text.Text
_KeyValue'StringValue
  = Data.ProtoLens.Prism.prism' KeyValue'StringValue
      (\ p__ ->
         case p__ of
             KeyValue'StringValue p__val -> Prelude.Just p__val
             _otherwise -> Prelude.Nothing)
_KeyValue'IntValue ::
                   Data.ProtoLens.Prism.Prism' KeyValue'Value Data.Int.Int64
_KeyValue'IntValue
  = Data.ProtoLens.Prism.prism' KeyValue'IntValue
      (\ p__ ->
         case p__ of
             KeyValue'IntValue p__val -> Prelude.Just p__val
             _otherwise -> Prelude.Nothing)
_KeyValue'DoubleValue ::
                      Data.ProtoLens.Prism.Prism' KeyValue'Value Prelude.Double
_KeyValue'DoubleValue
  = Data.ProtoLens.Prism.prism' KeyValue'DoubleValue
      (\ p__ ->
         case p__ of
             KeyValue'DoubleValue p__val -> Prelude.Just p__val
             _otherwise -> Prelude.Nothing)
_KeyValue'BoolValue ::
                    Data.ProtoLens.Prism.Prism' KeyValue'Value Prelude.Bool
_KeyValue'BoolValue
  = Data.ProtoLens.Prism.prism' KeyValue'BoolValue
      (\ p__ ->
         case p__ of
             KeyValue'BoolValue p__val -> Prelude.Just p__val
             _otherwise -> Prelude.Nothing)
_KeyValue'JsonValue ::
                    Data.ProtoLens.Prism.Prism' KeyValue'Value Data.Text.Text
_KeyValue'JsonValue
  = Data.ProtoLens.Prism.prism' KeyValue'JsonValue
      (\ p__ ->
         case p__ of
             KeyValue'JsonValue p__val -> Prelude.Just p__val
             _otherwise -> Prelude.Nothing)
{- | Fields :

    * 'Proto.Collector_Fields.timestamp' @:: Lens' Log Proto.Google.Protobuf.Timestamp.Timestamp@
    * 'Proto.Collector_Fields.maybe'timestamp' @:: Lens' Log (Prelude.Maybe Proto.Google.Protobuf.Timestamp.Timestamp)@
    * 'Proto.Collector_Fields.fields' @:: Lens' Log [KeyValue]@
    * 'Proto.Collector_Fields.vec'fields' @:: Lens' Log (Data.Vector.Vector KeyValue)@
 -}
data Log = Log{_Log'timestamp ::
               !(Prelude.Maybe Proto.Google.Protobuf.Timestamp.Timestamp),
               _Log'fields :: !(Data.Vector.Vector KeyValue),
               _Log'_unknownFields :: !Data.ProtoLens.FieldSet}
             deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Log where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Log "timestamp"
           (Proto.Google.Protobuf.Timestamp.Timestamp)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _Log'timestamp
               (\ x__ y__ -> x__{_Log'timestamp = y__}))
              Prelude.. Data.ProtoLens.maybeLens Data.ProtoLens.defMessage
instance Data.ProtoLens.Field.HasField Log "maybe'timestamp"
           (Prelude.Maybe Proto.Google.Protobuf.Timestamp.Timestamp)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _Log'timestamp
               (\ x__ y__ -> x__{_Log'timestamp = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField Log "fields" ([KeyValue])
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _Log'fields
               (\ x__ y__ -> x__{_Log'fields = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens Data.Vector.Generic.toList
                (\ _ y__ -> Data.Vector.Generic.fromList y__)
instance Data.ProtoLens.Field.HasField Log "vec'fields"
           (Data.Vector.Vector KeyValue)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _Log'fields
               (\ x__ y__ -> x__{_Log'fields = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Message Log where
        messageName _ = Data.Text.pack "lightstep.collector.Log"
        fieldsByTag
          = let timestamp__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "timestamp"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor
                           Proto.Google.Protobuf.Timestamp.Timestamp)
                      (Data.ProtoLens.OptionalField
                         (Data.ProtoLens.Field.field @"maybe'timestamp"))
                      :: Data.ProtoLens.FieldDescriptor Log
                fields__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "fields"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor KeyValue)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Data.ProtoLens.Field.field @"fields"))
                      :: Data.ProtoLens.FieldDescriptor Log
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, timestamp__field_descriptor),
                 (Data.ProtoLens.Tag 2, fields__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _Log'_unknownFields
              (\ x__ y__ -> x__{_Log'_unknownFields = y__})
        defMessage
          = Log{_Log'timestamp = Prelude.Nothing,
                _Log'fields = Data.Vector.Generic.empty,
                _Log'_unknownFields = ([])}
        parseMessage
          = let loop ::
                     Log ->
                       Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector
                         Data.ProtoLens.Encoding.Growing.RealWorld
                         KeyValue
                         -> Data.ProtoLens.Encoding.Bytes.Parser Log
                loop x mutable'fields
                  = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
                       if end then
                         do frozen'fields <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                               (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                  mutable'fields)
                            let missing = [] in
                              if Prelude.null missing then Prelude.return () else
                                Prelude.fail
                                  (("Missing required fields: ") Prelude.++
                                     Prelude.show (missing :: ([Prelude.String])))
                            Prelude.return
                              (Lens.Family2.over Data.ProtoLens.unknownFields
                                 (\ !t -> Prelude.reverse t)
                                 (Lens.Family2.set (Data.ProtoLens.Field.field @"vec'fields")
                                    frozen'fields
                                    x))
                         else
                         do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                            case tag of
                                10 -> do y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                  Data.ProtoLens.Encoding.Bytes.isolate
                                                    (Prelude.fromIntegral len)
                                                    Data.ProtoLens.parseMessage)
                                                Data.ProtoLens.Encoding.Bytes.<?> "timestamp"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"timestamp")
                                              y
                                              x)
                                           mutable'fields
                                18 -> do !y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                   Data.ProtoLens.Encoding.Bytes.isolate
                                                     (Prelude.fromIntegral len)
                                                     Data.ProtoLens.parseMessage)
                                                 Data.ProtoLens.Encoding.Bytes.<?> "fields"
                                         v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                (Data.ProtoLens.Encoding.Growing.append
                                                   mutable'fields
                                                   y)
                                         loop x v
                                wire -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                                   wire
                                           loop
                                             (Lens.Family2.over Data.ProtoLens.unknownFields
                                                (\ !t -> (:) y t)
                                                x)
                                             mutable'fields
              in
              (do mutable'fields <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                      Data.ProtoLens.Encoding.Growing.new
                  loop Data.ProtoLens.defMessage mutable'fields)
                Data.ProtoLens.Encoding.Bytes.<?> "Log"
        buildMessage
          = (\ _x ->
               (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'timestamp")
                    _x
                  of
                    (Prelude.Nothing) -> Data.Monoid.mempty
                    Prelude.Just _v -> (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                                         Data.Monoid.<>
                                         (((\ bs ->
                                              (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                 (Prelude.fromIntegral (Data.ByteString.length bs)))
                                                Data.Monoid.<>
                                                Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                            Prelude.. Data.ProtoLens.encodeMessage)
                                           _v)
                 Data.Monoid.<>
                 (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                    (\ _v ->
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 18) Data.Monoid.<>
                         (((\ bs ->
                              (Data.ProtoLens.Encoding.Bytes.putVarInt
                                 (Prelude.fromIntegral (Data.ByteString.length bs)))
                                Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            Prelude.. Data.ProtoLens.encodeMessage)
                           _v)
                    (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'fields") _x))
                   Data.Monoid.<>
                   Data.ProtoLens.Encoding.Wire.buildFieldSet
                     (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData Log where
        rnf
          = (\ x__ ->
               Control.DeepSeq.deepseq (_Log'_unknownFields x__)
                 (Control.DeepSeq.deepseq (_Log'timestamp x__)
                    (Control.DeepSeq.deepseq (_Log'fields x__) (()))))
{- | Fields :

    * 'Proto.Collector_Fields.name' @:: Lens' MetricsSample Data.Text.Text@
    * 'Proto.Collector_Fields.maybe'value' @:: Lens' MetricsSample (Prelude.Maybe MetricsSample'Value)@
    * 'Proto.Collector_Fields.maybe'intValue' @:: Lens' MetricsSample (Prelude.Maybe Data.Int.Int64)@
    * 'Proto.Collector_Fields.intValue' @:: Lens' MetricsSample Data.Int.Int64@
    * 'Proto.Collector_Fields.maybe'doubleValue' @:: Lens' MetricsSample (Prelude.Maybe Prelude.Double)@
    * 'Proto.Collector_Fields.doubleValue' @:: Lens' MetricsSample Prelude.Double@
 -}
data MetricsSample = MetricsSample{_MetricsSample'name ::
                                   !Data.Text.Text,
                                   _MetricsSample'value :: !(Prelude.Maybe MetricsSample'Value),
                                   _MetricsSample'_unknownFields :: !Data.ProtoLens.FieldSet}
                       deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show MetricsSample where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
data MetricsSample'Value = MetricsSample'IntValue !Data.Int.Int64
                         | MetricsSample'DoubleValue !Prelude.Double
                             deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField MetricsSample "name"
           (Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MetricsSample'name
               (\ x__ y__ -> x__{_MetricsSample'name = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField MetricsSample "maybe'value"
           (Prelude.Maybe MetricsSample'Value)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MetricsSample'value
               (\ x__ y__ -> x__{_MetricsSample'value = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField MetricsSample
           "maybe'intValue"
           (Prelude.Maybe Data.Int.Int64)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MetricsSample'value
               (\ x__ y__ -> x__{_MetricsSample'value = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens
                (\ x__ ->
                   case x__ of
                       Prelude.Just (MetricsSample'IntValue x__val) -> Prelude.Just x__val
                       _otherwise -> Prelude.Nothing)
                (\ _ y__ -> Prelude.fmap MetricsSample'IntValue y__)
instance Data.ProtoLens.Field.HasField MetricsSample "intValue"
           (Data.Int.Int64)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MetricsSample'value
               (\ x__ y__ -> x__{_MetricsSample'value = y__}))
              Prelude..
              (Lens.Family2.Unchecked.lens
                 (\ x__ ->
                    case x__ of
                        Prelude.Just (MetricsSample'IntValue x__val) -> Prelude.Just x__val
                        _otherwise -> Prelude.Nothing)
                 (\ _ y__ -> Prelude.fmap MetricsSample'IntValue y__))
                Prelude.. Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault
instance Data.ProtoLens.Field.HasField MetricsSample
           "maybe'doubleValue"
           (Prelude.Maybe Prelude.Double)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MetricsSample'value
               (\ x__ y__ -> x__{_MetricsSample'value = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens
                (\ x__ ->
                   case x__ of
                       Prelude.Just (MetricsSample'DoubleValue x__val) -> Prelude.Just
                                                                            x__val
                       _otherwise -> Prelude.Nothing)
                (\ _ y__ -> Prelude.fmap MetricsSample'DoubleValue y__)
instance Data.ProtoLens.Field.HasField MetricsSample "doubleValue"
           (Prelude.Double)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MetricsSample'value
               (\ x__ y__ -> x__{_MetricsSample'value = y__}))
              Prelude..
              (Lens.Family2.Unchecked.lens
                 (\ x__ ->
                    case x__ of
                        Prelude.Just (MetricsSample'DoubleValue x__val) -> Prelude.Just
                                                                             x__val
                        _otherwise -> Prelude.Nothing)
                 (\ _ y__ -> Prelude.fmap MetricsSample'DoubleValue y__))
                Prelude.. Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault
instance Data.ProtoLens.Message MetricsSample where
        messageName _ = Data.Text.pack "lightstep.collector.MetricsSample"
        fieldsByTag
          = let name__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "name"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"name"))
                      :: Data.ProtoLens.FieldDescriptor MetricsSample
                intValue__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "int_value"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
                      (Data.ProtoLens.OptionalField
                         (Data.ProtoLens.Field.field @"maybe'intValue"))
                      :: Data.ProtoLens.FieldDescriptor MetricsSample
                doubleValue__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "double_value"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.DoubleField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Double)
                      (Data.ProtoLens.OptionalField
                         (Data.ProtoLens.Field.field @"maybe'doubleValue"))
                      :: Data.ProtoLens.FieldDescriptor MetricsSample
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, name__field_descriptor),
                 (Data.ProtoLens.Tag 2, intValue__field_descriptor),
                 (Data.ProtoLens.Tag 3, doubleValue__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _MetricsSample'_unknownFields
              (\ x__ y__ -> x__{_MetricsSample'_unknownFields = y__})
        defMessage
          = MetricsSample{_MetricsSample'name = Data.ProtoLens.fieldDefault,
                          _MetricsSample'value = Prelude.Nothing,
                          _MetricsSample'_unknownFields = ([])}
        parseMessage
          = let loop ::
                     MetricsSample -> Data.ProtoLens.Encoding.Bytes.Parser MetricsSample
                loop x
                  = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
                       if end then
                         do let missing = [] in
                              if Prelude.null missing then Prelude.return () else
                                Prelude.fail
                                  (("Missing required fields: ") Prelude.++
                                     Prelude.show (missing :: ([Prelude.String])))
                            Prelude.return
                              (Lens.Family2.over Data.ProtoLens.unknownFields
                                 (\ !t -> Prelude.reverse t)
                                 x)
                         else
                         do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                            case tag of
                                10 -> do y <- (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                              Data.ProtoLens.Encoding.Bytes.getBytes
                                                                (Prelude.fromIntegral len)
                                                  Data.ProtoLens.Encoding.Bytes.runEither
                                                    (case Data.Text.Encoding.decodeUtf8' value of
                                                         Prelude.Left err -> Prelude.Left
                                                                               (Prelude.show err)
                                                         Prelude.Right r -> Prelude.Right r))
                                                Data.ProtoLens.Encoding.Bytes.<?> "name"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"name") y
                                              x)
                                16 -> do y <- (Prelude.fmap Prelude.fromIntegral
                                                 Data.ProtoLens.Encoding.Bytes.getVarInt)
                                                Data.ProtoLens.Encoding.Bytes.<?> "int_value"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"intValue")
                                              y
                                              x)
                                25 -> do y <- (Prelude.fmap
                                                 Data.ProtoLens.Encoding.Bytes.wordToDouble
                                                 Data.ProtoLens.Encoding.Bytes.getFixed64)
                                                Data.ProtoLens.Encoding.Bytes.<?> "double_value"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"doubleValue")
                                              y
                                              x)
                                wire -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                                   wire
                                           loop
                                             (Lens.Family2.over Data.ProtoLens.unknownFields
                                                (\ !t -> (:) y t)
                                                x)
              in
              (do loop Data.ProtoLens.defMessage)
                Data.ProtoLens.Encoding.Bytes.<?> "MetricsSample"
        buildMessage
          = (\ _x ->
               (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"name") _x
                  in
                  if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty else
                    (Data.ProtoLens.Encoding.Bytes.putVarInt 10) Data.Monoid.<>
                      (((\ bs ->
                           (Data.ProtoLens.Encoding.Bytes.putVarInt
                              (Prelude.fromIntegral (Data.ByteString.length bs)))
                             Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Prelude.. Data.Text.Encoding.encodeUtf8)
                        _v)
                 Data.Monoid.<>
                 (case
                    Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'value") _x of
                      (Prelude.Nothing) -> Data.Monoid.mempty
                      Prelude.Just
                        (MetricsSample'IntValue
                           v) -> (Data.ProtoLens.Encoding.Bytes.putVarInt 16) Data.Monoid.<>
                                   ((Data.ProtoLens.Encoding.Bytes.putVarInt) Prelude..
                                      Prelude.fromIntegral)
                                     v
                      Prelude.Just
                        (MetricsSample'DoubleValue
                           v) -> (Data.ProtoLens.Encoding.Bytes.putVarInt 25) Data.Monoid.<>
                                   ((Data.ProtoLens.Encoding.Bytes.putFixed64) Prelude..
                                      Data.ProtoLens.Encoding.Bytes.doubleToWord)
                                     v)
                   Data.Monoid.<>
                   Data.ProtoLens.Encoding.Wire.buildFieldSet
                     (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData MetricsSample where
        rnf
          = (\ x__ ->
               Control.DeepSeq.deepseq (_MetricsSample'_unknownFields x__)
                 (Control.DeepSeq.deepseq (_MetricsSample'name x__)
                    (Control.DeepSeq.deepseq (_MetricsSample'value x__) (()))))
instance Control.DeepSeq.NFData MetricsSample'Value where
        rnf (MetricsSample'IntValue x__) = Control.DeepSeq.rnf x__
        rnf (MetricsSample'DoubleValue x__) = Control.DeepSeq.rnf x__
_MetricsSample'IntValue ::
                        Data.ProtoLens.Prism.Prism' MetricsSample'Value Data.Int.Int64
_MetricsSample'IntValue
  = Data.ProtoLens.Prism.prism' MetricsSample'IntValue
      (\ p__ ->
         case p__ of
             MetricsSample'IntValue p__val -> Prelude.Just p__val
             _otherwise -> Prelude.Nothing)
_MetricsSample'DoubleValue ::
                           Data.ProtoLens.Prism.Prism' MetricsSample'Value Prelude.Double
_MetricsSample'DoubleValue
  = Data.ProtoLens.Prism.prism' MetricsSample'DoubleValue
      (\ p__ ->
         case p__ of
             MetricsSample'DoubleValue p__val -> Prelude.Just p__val
             _otherwise -> Prelude.Nothing)
{- | Fields :

    * 'Proto.Collector_Fields.relationship' @:: Lens' Reference Reference'Relationship@
    * 'Proto.Collector_Fields.spanContext' @:: Lens' Reference SpanContext@
    * 'Proto.Collector_Fields.maybe'spanContext' @:: Lens' Reference (Prelude.Maybe SpanContext)@
 -}
data Reference = Reference{_Reference'relationship ::
                           !Reference'Relationship,
                           _Reference'spanContext :: !(Prelude.Maybe SpanContext),
                           _Reference'_unknownFields :: !Data.ProtoLens.FieldSet}
                   deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Reference where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Reference "relationship"
           (Reference'Relationship)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _Reference'relationship
               (\ x__ y__ -> x__{_Reference'relationship = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField Reference "spanContext"
           (SpanContext)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _Reference'spanContext
               (\ x__ y__ -> x__{_Reference'spanContext = y__}))
              Prelude.. Data.ProtoLens.maybeLens Data.ProtoLens.defMessage
instance Data.ProtoLens.Field.HasField Reference
           "maybe'spanContext"
           (Prelude.Maybe SpanContext)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _Reference'spanContext
               (\ x__ y__ -> x__{_Reference'spanContext = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Message Reference where
        messageName _ = Data.Text.pack "lightstep.collector.Reference"
        fieldsByTag
          = let relationship__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "relationship"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                         Data.ProtoLens.FieldTypeDescriptor Reference'Relationship)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"relationship"))
                      :: Data.ProtoLens.FieldDescriptor Reference
                spanContext__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "span_context"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor SpanContext)
                      (Data.ProtoLens.OptionalField
                         (Data.ProtoLens.Field.field @"maybe'spanContext"))
                      :: Data.ProtoLens.FieldDescriptor Reference
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, relationship__field_descriptor),
                 (Data.ProtoLens.Tag 2, spanContext__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _Reference'_unknownFields
              (\ x__ y__ -> x__{_Reference'_unknownFields = y__})
        defMessage
          = Reference{_Reference'relationship = Data.ProtoLens.fieldDefault,
                      _Reference'spanContext = Prelude.Nothing,
                      _Reference'_unknownFields = ([])}
        parseMessage
          = let loop ::
                     Reference -> Data.ProtoLens.Encoding.Bytes.Parser Reference
                loop x
                  = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
                       if end then
                         do let missing = [] in
                              if Prelude.null missing then Prelude.return () else
                                Prelude.fail
                                  (("Missing required fields: ") Prelude.++
                                     Prelude.show (missing :: ([Prelude.String])))
                            Prelude.return
                              (Lens.Family2.over Data.ProtoLens.unknownFields
                                 (\ !t -> Prelude.reverse t)
                                 x)
                         else
                         do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                            case tag of
                                8 -> do y <- (Prelude.fmap Prelude.toEnum
                                                (Prelude.fmap Prelude.fromIntegral
                                                   Data.ProtoLens.Encoding.Bytes.getVarInt))
                                               Data.ProtoLens.Encoding.Bytes.<?> "relationship"
                                        loop
                                          (Lens.Family2.set
                                             (Data.ProtoLens.Field.field @"relationship")
                                             y
                                             x)
                                18 -> do y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                  Data.ProtoLens.Encoding.Bytes.isolate
                                                    (Prelude.fromIntegral len)
                                                    Data.ProtoLens.parseMessage)
                                                Data.ProtoLens.Encoding.Bytes.<?> "span_context"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"spanContext")
                                              y
                                              x)
                                wire -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                                   wire
                                           loop
                                             (Lens.Family2.over Data.ProtoLens.unknownFields
                                                (\ !t -> (:) y t)
                                                x)
              in
              (do loop Data.ProtoLens.defMessage)
                Data.ProtoLens.Encoding.Bytes.<?> "Reference"
        buildMessage
          = (\ _x ->
               (let _v
                      = Lens.Family2.view (Data.ProtoLens.Field.field @"relationship") _x
                  in
                  if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty else
                    (Data.ProtoLens.Encoding.Bytes.putVarInt 8) Data.Monoid.<>
                      (((Data.ProtoLens.Encoding.Bytes.putVarInt) Prelude..
                          Prelude.fromIntegral)
                         Prelude.. Prelude.fromEnum)
                        _v)
                 Data.Monoid.<>
                 (case
                    Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'spanContext")
                      _x
                    of
                      (Prelude.Nothing) -> Data.Monoid.mempty
                      Prelude.Just _v -> (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                                           Data.Monoid.<>
                                           (((\ bs ->
                                                (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                   (Prelude.fromIntegral
                                                      (Data.ByteString.length bs)))
                                                  Data.Monoid.<>
                                                  Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                              Prelude.. Data.ProtoLens.encodeMessage)
                                             _v)
                   Data.Monoid.<>
                   Data.ProtoLens.Encoding.Wire.buildFieldSet
                     (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData Reference where
        rnf
          = (\ x__ ->
               Control.DeepSeq.deepseq (_Reference'_unknownFields x__)
                 (Control.DeepSeq.deepseq (_Reference'relationship x__)
                    (Control.DeepSeq.deepseq (_Reference'spanContext x__) (()))))
newtype Reference'Relationship'UnrecognizedValue = Reference'Relationship'UnrecognizedValue Data.Int.Int32
                                                     deriving (Prelude.Eq, Prelude.Ord,
                                                               Prelude.Show)
data Reference'Relationship = Reference'CHILD_OF
                            | Reference'FOLLOWS_FROM
                            | Reference'Relationship'Unrecognized !Reference'Relationship'UnrecognizedValue
                                deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.MessageEnum Reference'Relationship where
        maybeToEnum 0 = Prelude.Just Reference'CHILD_OF
        maybeToEnum 1 = Prelude.Just Reference'FOLLOWS_FROM
        maybeToEnum k
          = Prelude.Just
              (Reference'Relationship'Unrecognized
                 (Reference'Relationship'UnrecognizedValue
                    (Prelude.fromIntegral k)))
        showEnum Reference'CHILD_OF = "CHILD_OF"
        showEnum Reference'FOLLOWS_FROM = "FOLLOWS_FROM"
        showEnum
          (Reference'Relationship'Unrecognized
             (Reference'Relationship'UnrecognizedValue k))
          = Prelude.show k
        readEnum k
          | (k) Prelude.== "CHILD_OF" = Prelude.Just Reference'CHILD_OF
          | (k) Prelude.== "FOLLOWS_FROM" =
            Prelude.Just Reference'FOLLOWS_FROM
        readEnum k
          = (Text.Read.readMaybe k) Prelude.>>= Data.ProtoLens.maybeToEnum
instance Prelude.Bounded Reference'Relationship where
        minBound = Reference'CHILD_OF
        maxBound = Reference'FOLLOWS_FROM
instance Prelude.Enum Reference'Relationship where
        toEnum k__
          = Prelude.maybe
              (Prelude.error
                 (("toEnum: unknown value for enum Relationship: ") Prelude.++
                    Prelude.show k__))
              Prelude.id
              (Data.ProtoLens.maybeToEnum k__)
        fromEnum Reference'CHILD_OF = 0
        fromEnum Reference'FOLLOWS_FROM = 1
        fromEnum
          (Reference'Relationship'Unrecognized
             (Reference'Relationship'UnrecognizedValue k))
          = Prelude.fromIntegral k
        succ Reference'FOLLOWS_FROM
          = Prelude.error
              "Reference'Relationship.succ: bad argument Reference'FOLLOWS_FROM. This value would be out of bounds."
        succ Reference'CHILD_OF = Reference'FOLLOWS_FROM
        succ (Reference'Relationship'Unrecognized _)
          = Prelude.error
              "Reference'Relationship.succ: bad argument: unrecognized value"
        pred Reference'CHILD_OF
          = Prelude.error
              "Reference'Relationship.pred: bad argument Reference'CHILD_OF. This value would be out of bounds."
        pred Reference'FOLLOWS_FROM = Reference'CHILD_OF
        pred (Reference'Relationship'Unrecognized _)
          = Prelude.error
              "Reference'Relationship.pred: bad argument: unrecognized value"
        enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
        enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
        enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
        enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.ProtoLens.FieldDefault Reference'Relationship where
        fieldDefault = Reference'CHILD_OF
instance Control.DeepSeq.NFData Reference'Relationship where
        rnf x__ = Prelude.seq x__ (())
{- | Fields :

    * 'Proto.Collector_Fields.reporter' @:: Lens' ReportRequest Reporter@
    * 'Proto.Collector_Fields.maybe'reporter' @:: Lens' ReportRequest (Prelude.Maybe Reporter)@
    * 'Proto.Collector_Fields.auth' @:: Lens' ReportRequest Auth@
    * 'Proto.Collector_Fields.maybe'auth' @:: Lens' ReportRequest (Prelude.Maybe Auth)@
    * 'Proto.Collector_Fields.spans' @:: Lens' ReportRequest [Span]@
    * 'Proto.Collector_Fields.vec'spans' @:: Lens' ReportRequest (Data.Vector.Vector Span)@
    * 'Proto.Collector_Fields.timestampOffsetMicros' @:: Lens' ReportRequest Data.Int.Int64@
    * 'Proto.Collector_Fields.internalMetrics' @:: Lens' ReportRequest InternalMetrics@
    * 'Proto.Collector_Fields.maybe'internalMetrics' @:: Lens' ReportRequest (Prelude.Maybe InternalMetrics)@
 -}
data ReportRequest = ReportRequest{_ReportRequest'reporter ::
                                   !(Prelude.Maybe Reporter),
                                   _ReportRequest'auth :: !(Prelude.Maybe Auth),
                                   _ReportRequest'spans :: !(Data.Vector.Vector Span),
                                   _ReportRequest'timestampOffsetMicros :: !Data.Int.Int64,
                                   _ReportRequest'internalMetrics ::
                                   !(Prelude.Maybe InternalMetrics),
                                   _ReportRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
                       deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ReportRequest where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ReportRequest "reporter"
           (Reporter)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _ReportRequest'reporter
               (\ x__ y__ -> x__{_ReportRequest'reporter = y__}))
              Prelude.. Data.ProtoLens.maybeLens Data.ProtoLens.defMessage
instance Data.ProtoLens.Field.HasField ReportRequest
           "maybe'reporter"
           (Prelude.Maybe Reporter)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _ReportRequest'reporter
               (\ x__ y__ -> x__{_ReportRequest'reporter = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField ReportRequest "auth" (Auth)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _ReportRequest'auth
               (\ x__ y__ -> x__{_ReportRequest'auth = y__}))
              Prelude.. Data.ProtoLens.maybeLens Data.ProtoLens.defMessage
instance Data.ProtoLens.Field.HasField ReportRequest "maybe'auth"
           (Prelude.Maybe Auth)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _ReportRequest'auth
               (\ x__ y__ -> x__{_ReportRequest'auth = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField ReportRequest "spans"
           ([Span])
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _ReportRequest'spans
               (\ x__ y__ -> x__{_ReportRequest'spans = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens Data.Vector.Generic.toList
                (\ _ y__ -> Data.Vector.Generic.fromList y__)
instance Data.ProtoLens.Field.HasField ReportRequest "vec'spans"
           (Data.Vector.Vector Span)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _ReportRequest'spans
               (\ x__ y__ -> x__{_ReportRequest'spans = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField ReportRequest
           "timestampOffsetMicros"
           (Data.Int.Int64)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _ReportRequest'timestampOffsetMicros
               (\ x__ y__ -> x__{_ReportRequest'timestampOffsetMicros = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField ReportRequest
           "internalMetrics"
           (InternalMetrics)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _ReportRequest'internalMetrics
               (\ x__ y__ -> x__{_ReportRequest'internalMetrics = y__}))
              Prelude.. Data.ProtoLens.maybeLens Data.ProtoLens.defMessage
instance Data.ProtoLens.Field.HasField ReportRequest
           "maybe'internalMetrics"
           (Prelude.Maybe InternalMetrics)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _ReportRequest'internalMetrics
               (\ x__ y__ -> x__{_ReportRequest'internalMetrics = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Message ReportRequest where
        messageName _ = Data.Text.pack "lightstep.collector.ReportRequest"
        fieldsByTag
          = let reporter__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "reporter"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor Reporter)
                      (Data.ProtoLens.OptionalField
                         (Data.ProtoLens.Field.field @"maybe'reporter"))
                      :: Data.ProtoLens.FieldDescriptor ReportRequest
                auth__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "auth"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor Auth)
                      (Data.ProtoLens.OptionalField
                         (Data.ProtoLens.Field.field @"maybe'auth"))
                      :: Data.ProtoLens.FieldDescriptor ReportRequest
                spans__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "spans"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor Span)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Data.ProtoLens.Field.field @"spans"))
                      :: Data.ProtoLens.FieldDescriptor ReportRequest
                timestampOffsetMicros__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "timestamp_offset_micros"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"timestampOffsetMicros"))
                      :: Data.ProtoLens.FieldDescriptor ReportRequest
                internalMetrics__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "internal_metrics"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor InternalMetrics)
                      (Data.ProtoLens.OptionalField
                         (Data.ProtoLens.Field.field @"maybe'internalMetrics"))
                      :: Data.ProtoLens.FieldDescriptor ReportRequest
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, reporter__field_descriptor),
                 (Data.ProtoLens.Tag 2, auth__field_descriptor),
                 (Data.ProtoLens.Tag 3, spans__field_descriptor),
                 (Data.ProtoLens.Tag 5, timestampOffsetMicros__field_descriptor),
                 (Data.ProtoLens.Tag 6, internalMetrics__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _ReportRequest'_unknownFields
              (\ x__ y__ -> x__{_ReportRequest'_unknownFields = y__})
        defMessage
          = ReportRequest{_ReportRequest'reporter = Prelude.Nothing,
                          _ReportRequest'auth = Prelude.Nothing,
                          _ReportRequest'spans = Data.Vector.Generic.empty,
                          _ReportRequest'timestampOffsetMicros = Data.ProtoLens.fieldDefault,
                          _ReportRequest'internalMetrics = Prelude.Nothing,
                          _ReportRequest'_unknownFields = ([])}
        parseMessage
          = let loop ::
                     ReportRequest ->
                       Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector
                         Data.ProtoLens.Encoding.Growing.RealWorld
                         Span
                         -> Data.ProtoLens.Encoding.Bytes.Parser ReportRequest
                loop x mutable'spans
                  = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
                       if end then
                         do frozen'spans <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                              (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                 mutable'spans)
                            let missing = [] in
                              if Prelude.null missing then Prelude.return () else
                                Prelude.fail
                                  (("Missing required fields: ") Prelude.++
                                     Prelude.show (missing :: ([Prelude.String])))
                            Prelude.return
                              (Lens.Family2.over Data.ProtoLens.unknownFields
                                 (\ !t -> Prelude.reverse t)
                                 (Lens.Family2.set (Data.ProtoLens.Field.field @"vec'spans")
                                    frozen'spans
                                    x))
                         else
                         do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                            case tag of
                                10 -> do y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                  Data.ProtoLens.Encoding.Bytes.isolate
                                                    (Prelude.fromIntegral len)
                                                    Data.ProtoLens.parseMessage)
                                                Data.ProtoLens.Encoding.Bytes.<?> "reporter"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"reporter")
                                              y
                                              x)
                                           mutable'spans
                                18 -> do y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                  Data.ProtoLens.Encoding.Bytes.isolate
                                                    (Prelude.fromIntegral len)
                                                    Data.ProtoLens.parseMessage)
                                                Data.ProtoLens.Encoding.Bytes.<?> "auth"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"auth") y
                                              x)
                                           mutable'spans
                                26 -> do !y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                   Data.ProtoLens.Encoding.Bytes.isolate
                                                     (Prelude.fromIntegral len)
                                                     Data.ProtoLens.parseMessage)
                                                 Data.ProtoLens.Encoding.Bytes.<?> "spans"
                                         v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                (Data.ProtoLens.Encoding.Growing.append
                                                   mutable'spans
                                                   y)
                                         loop x v
                                40 -> do y <- (Prelude.fmap Prelude.fromIntegral
                                                 Data.ProtoLens.Encoding.Bytes.getVarInt)
                                                Data.ProtoLens.Encoding.Bytes.<?>
                                                "timestamp_offset_micros"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"timestampOffsetMicros")
                                              y
                                              x)
                                           mutable'spans
                                50 -> do y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                  Data.ProtoLens.Encoding.Bytes.isolate
                                                    (Prelude.fromIntegral len)
                                                    Data.ProtoLens.parseMessage)
                                                Data.ProtoLens.Encoding.Bytes.<?> "internal_metrics"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"internalMetrics")
                                              y
                                              x)
                                           mutable'spans
                                wire -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                                   wire
                                           loop
                                             (Lens.Family2.over Data.ProtoLens.unknownFields
                                                (\ !t -> (:) y t)
                                                x)
                                             mutable'spans
              in
              (do mutable'spans <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                     Data.ProtoLens.Encoding.Growing.new
                  loop Data.ProtoLens.defMessage mutable'spans)
                Data.ProtoLens.Encoding.Bytes.<?> "ReportRequest"
        buildMessage
          = (\ _x ->
               (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'reporter") _x
                  of
                    (Prelude.Nothing) -> Data.Monoid.mempty
                    Prelude.Just _v -> (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                                         Data.Monoid.<>
                                         (((\ bs ->
                                              (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                 (Prelude.fromIntegral (Data.ByteString.length bs)))
                                                Data.Monoid.<>
                                                Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                            Prelude.. Data.ProtoLens.encodeMessage)
                                           _v)
                 Data.Monoid.<>
                 (case
                    Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'auth") _x of
                      (Prelude.Nothing) -> Data.Monoid.mempty
                      Prelude.Just _v -> (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                                           Data.Monoid.<>
                                           (((\ bs ->
                                                (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                   (Prelude.fromIntegral
                                                      (Data.ByteString.length bs)))
                                                  Data.Monoid.<>
                                                  Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                              Prelude.. Data.ProtoLens.encodeMessage)
                                             _v)
                   Data.Monoid.<>
                   (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                      (\ _v ->
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 26) Data.Monoid.<>
                           (((\ bs ->
                                (Data.ProtoLens.Encoding.Bytes.putVarInt
                                   (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                              Prelude.. Data.ProtoLens.encodeMessage)
                             _v)
                      (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'spans") _x))
                     Data.Monoid.<>
                     (let _v
                            = Lens.Family2.view
                                (Data.ProtoLens.Field.field @"timestampOffsetMicros")
                                _x
                        in
                        if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty else
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 40) Data.Monoid.<>
                            ((Data.ProtoLens.Encoding.Bytes.putVarInt) Prelude..
                               Prelude.fromIntegral)
                              _v)
                       Data.Monoid.<>
                       (case
                          Lens.Family2.view
                            (Data.ProtoLens.Field.field @"maybe'internalMetrics")
                            _x
                          of
                            (Prelude.Nothing) -> Data.Monoid.mempty
                            Prelude.Just _v -> (Data.ProtoLens.Encoding.Bytes.putVarInt 50)
                                                 Data.Monoid.<>
                                                 (((\ bs ->
                                                      (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                         (Prelude.fromIntegral
                                                            (Data.ByteString.length bs)))
                                                        Data.Monoid.<>
                                                        Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                                    Prelude.. Data.ProtoLens.encodeMessage)
                                                   _v)
                         Data.Monoid.<>
                         Data.ProtoLens.Encoding.Wire.buildFieldSet
                           (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData ReportRequest where
        rnf
          = (\ x__ ->
               Control.DeepSeq.deepseq (_ReportRequest'_unknownFields x__)
                 (Control.DeepSeq.deepseq (_ReportRequest'reporter x__)
                    (Control.DeepSeq.deepseq (_ReportRequest'auth x__)
                       (Control.DeepSeq.deepseq (_ReportRequest'spans x__)
                          (Control.DeepSeq.deepseq (_ReportRequest'timestampOffsetMicros x__)
                             (Control.DeepSeq.deepseq (_ReportRequest'internalMetrics x__)
                                (())))))))
{- | Fields :

    * 'Proto.Collector_Fields.commands' @:: Lens' ReportResponse [Command]@
    * 'Proto.Collector_Fields.vec'commands' @:: Lens' ReportResponse (Data.Vector.Vector Command)@
    * 'Proto.Collector_Fields.receiveTimestamp' @:: Lens' ReportResponse Proto.Google.Protobuf.Timestamp.Timestamp@
    * 'Proto.Collector_Fields.maybe'receiveTimestamp' @:: Lens' ReportResponse
  (Prelude.Maybe Proto.Google.Protobuf.Timestamp.Timestamp)@
    * 'Proto.Collector_Fields.transmitTimestamp' @:: Lens' ReportResponse Proto.Google.Protobuf.Timestamp.Timestamp@
    * 'Proto.Collector_Fields.maybe'transmitTimestamp' @:: Lens' ReportResponse
  (Prelude.Maybe Proto.Google.Protobuf.Timestamp.Timestamp)@
    * 'Proto.Collector_Fields.errors' @:: Lens' ReportResponse [Data.Text.Text]@
    * 'Proto.Collector_Fields.vec'errors' @:: Lens' ReportResponse (Data.Vector.Vector Data.Text.Text)@
    * 'Proto.Collector_Fields.warnings' @:: Lens' ReportResponse [Data.Text.Text]@
    * 'Proto.Collector_Fields.vec'warnings' @:: Lens' ReportResponse (Data.Vector.Vector Data.Text.Text)@
    * 'Proto.Collector_Fields.infos' @:: Lens' ReportResponse [Data.Text.Text]@
    * 'Proto.Collector_Fields.vec'infos' @:: Lens' ReportResponse (Data.Vector.Vector Data.Text.Text)@
 -}
data ReportResponse = ReportResponse{_ReportResponse'commands ::
                                     !(Data.Vector.Vector Command),
                                     _ReportResponse'receiveTimestamp ::
                                     !(Prelude.Maybe Proto.Google.Protobuf.Timestamp.Timestamp),
                                     _ReportResponse'transmitTimestamp ::
                                     !(Prelude.Maybe Proto.Google.Protobuf.Timestamp.Timestamp),
                                     _ReportResponse'errors :: !(Data.Vector.Vector Data.Text.Text),
                                     _ReportResponse'warnings ::
                                     !(Data.Vector.Vector Data.Text.Text),
                                     _ReportResponse'infos :: !(Data.Vector.Vector Data.Text.Text),
                                     _ReportResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
                        deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ReportResponse where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ReportResponse "commands"
           ([Command])
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _ReportResponse'commands
               (\ x__ y__ -> x__{_ReportResponse'commands = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens Data.Vector.Generic.toList
                (\ _ y__ -> Data.Vector.Generic.fromList y__)
instance Data.ProtoLens.Field.HasField ReportResponse
           "vec'commands"
           (Data.Vector.Vector Command)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _ReportResponse'commands
               (\ x__ y__ -> x__{_ReportResponse'commands = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField ReportResponse
           "receiveTimestamp"
           (Proto.Google.Protobuf.Timestamp.Timestamp)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _ReportResponse'receiveTimestamp
               (\ x__ y__ -> x__{_ReportResponse'receiveTimestamp = y__}))
              Prelude.. Data.ProtoLens.maybeLens Data.ProtoLens.defMessage
instance Data.ProtoLens.Field.HasField ReportResponse
           "maybe'receiveTimestamp"
           (Prelude.Maybe Proto.Google.Protobuf.Timestamp.Timestamp)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _ReportResponse'receiveTimestamp
               (\ x__ y__ -> x__{_ReportResponse'receiveTimestamp = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField ReportResponse
           "transmitTimestamp"
           (Proto.Google.Protobuf.Timestamp.Timestamp)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _ReportResponse'transmitTimestamp
               (\ x__ y__ -> x__{_ReportResponse'transmitTimestamp = y__}))
              Prelude.. Data.ProtoLens.maybeLens Data.ProtoLens.defMessage
instance Data.ProtoLens.Field.HasField ReportResponse
           "maybe'transmitTimestamp"
           (Prelude.Maybe Proto.Google.Protobuf.Timestamp.Timestamp)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _ReportResponse'transmitTimestamp
               (\ x__ y__ -> x__{_ReportResponse'transmitTimestamp = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField ReportResponse "errors"
           ([Data.Text.Text])
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _ReportResponse'errors
               (\ x__ y__ -> x__{_ReportResponse'errors = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens Data.Vector.Generic.toList
                (\ _ y__ -> Data.Vector.Generic.fromList y__)
instance Data.ProtoLens.Field.HasField ReportResponse "vec'errors"
           (Data.Vector.Vector Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _ReportResponse'errors
               (\ x__ y__ -> x__{_ReportResponse'errors = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField ReportResponse "warnings"
           ([Data.Text.Text])
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _ReportResponse'warnings
               (\ x__ y__ -> x__{_ReportResponse'warnings = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens Data.Vector.Generic.toList
                (\ _ y__ -> Data.Vector.Generic.fromList y__)
instance Data.ProtoLens.Field.HasField ReportResponse
           "vec'warnings"
           (Data.Vector.Vector Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _ReportResponse'warnings
               (\ x__ y__ -> x__{_ReportResponse'warnings = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField ReportResponse "infos"
           ([Data.Text.Text])
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _ReportResponse'infos
               (\ x__ y__ -> x__{_ReportResponse'infos = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens Data.Vector.Generic.toList
                (\ _ y__ -> Data.Vector.Generic.fromList y__)
instance Data.ProtoLens.Field.HasField ReportResponse "vec'infos"
           (Data.Vector.Vector Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _ReportResponse'infos
               (\ x__ y__ -> x__{_ReportResponse'infos = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Message ReportResponse where
        messageName _ = Data.Text.pack "lightstep.collector.ReportResponse"
        fieldsByTag
          = let commands__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "commands"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor Command)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Data.ProtoLens.Field.field @"commands"))
                      :: Data.ProtoLens.FieldDescriptor ReportResponse
                receiveTimestamp__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "receive_timestamp"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor
                           Proto.Google.Protobuf.Timestamp.Timestamp)
                      (Data.ProtoLens.OptionalField
                         (Data.ProtoLens.Field.field @"maybe'receiveTimestamp"))
                      :: Data.ProtoLens.FieldDescriptor ReportResponse
                transmitTimestamp__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "transmit_timestamp"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor
                           Proto.Google.Protobuf.Timestamp.Timestamp)
                      (Data.ProtoLens.OptionalField
                         (Data.ProtoLens.Field.field @"maybe'transmitTimestamp"))
                      :: Data.ProtoLens.FieldDescriptor ReportResponse
                errors__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "errors"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Data.ProtoLens.Field.field @"errors"))
                      :: Data.ProtoLens.FieldDescriptor ReportResponse
                warnings__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "warnings"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Data.ProtoLens.Field.field @"warnings"))
                      :: Data.ProtoLens.FieldDescriptor ReportResponse
                infos__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "infos"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Data.ProtoLens.Field.field @"infos"))
                      :: Data.ProtoLens.FieldDescriptor ReportResponse
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, commands__field_descriptor),
                 (Data.ProtoLens.Tag 2, receiveTimestamp__field_descriptor),
                 (Data.ProtoLens.Tag 3, transmitTimestamp__field_descriptor),
                 (Data.ProtoLens.Tag 4, errors__field_descriptor),
                 (Data.ProtoLens.Tag 5, warnings__field_descriptor),
                 (Data.ProtoLens.Tag 6, infos__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _ReportResponse'_unknownFields
              (\ x__ y__ -> x__{_ReportResponse'_unknownFields = y__})
        defMessage
          = ReportResponse{_ReportResponse'commands =
                             Data.Vector.Generic.empty,
                           _ReportResponse'receiveTimestamp = Prelude.Nothing,
                           _ReportResponse'transmitTimestamp = Prelude.Nothing,
                           _ReportResponse'errors = Data.Vector.Generic.empty,
                           _ReportResponse'warnings = Data.Vector.Generic.empty,
                           _ReportResponse'infos = Data.Vector.Generic.empty,
                           _ReportResponse'_unknownFields = ([])}
        parseMessage
          = let loop ::
                     ReportResponse ->
                       Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector
                         Data.ProtoLens.Encoding.Growing.RealWorld
                         Command
                         ->
                         Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector
                           Data.ProtoLens.Encoding.Growing.RealWorld
                           Data.Text.Text
                           ->
                           Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector
                             Data.ProtoLens.Encoding.Growing.RealWorld
                             Data.Text.Text
                             ->
                             Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector
                               Data.ProtoLens.Encoding.Growing.RealWorld
                               Data.Text.Text
                               -> Data.ProtoLens.Encoding.Bytes.Parser ReportResponse
                loop x mutable'commands mutable'errors mutable'infos
                  mutable'warnings
                  = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
                       if end then
                         do frozen'commands <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                 (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                    mutable'commands)
                            frozen'errors <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                               (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                  mutable'errors)
                            frozen'infos <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                              (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                 mutable'infos)
                            frozen'warnings <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                 (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                    mutable'warnings)
                            let missing = [] in
                              if Prelude.null missing then Prelude.return () else
                                Prelude.fail
                                  (("Missing required fields: ") Prelude.++
                                     Prelude.show (missing :: ([Prelude.String])))
                            Prelude.return
                              (Lens.Family2.over Data.ProtoLens.unknownFields
                                 (\ !t -> Prelude.reverse t)
                                 (Lens.Family2.set (Data.ProtoLens.Field.field @"vec'commands")
                                    frozen'commands
                                    (Lens.Family2.set (Data.ProtoLens.Field.field @"vec'errors")
                                       frozen'errors
                                       (Lens.Family2.set (Data.ProtoLens.Field.field @"vec'infos")
                                          frozen'infos
                                          (Lens.Family2.set
                                             (Data.ProtoLens.Field.field @"vec'warnings")
                                             frozen'warnings
                                             x)))))
                         else
                         do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                            case tag of
                                10 -> do !y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                   Data.ProtoLens.Encoding.Bytes.isolate
                                                     (Prelude.fromIntegral len)
                                                     Data.ProtoLens.parseMessage)
                                                 Data.ProtoLens.Encoding.Bytes.<?> "commands"
                                         v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                (Data.ProtoLens.Encoding.Growing.append
                                                   mutable'commands
                                                   y)
                                         loop x v mutable'errors mutable'infos mutable'warnings
                                18 -> do y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                  Data.ProtoLens.Encoding.Bytes.isolate
                                                    (Prelude.fromIntegral len)
                                                    Data.ProtoLens.parseMessage)
                                                Data.ProtoLens.Encoding.Bytes.<?>
                                                "receive_timestamp"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"receiveTimestamp")
                                              y
                                              x)
                                           mutable'commands
                                           mutable'errors
                                           mutable'infos
                                           mutable'warnings
                                26 -> do y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                  Data.ProtoLens.Encoding.Bytes.isolate
                                                    (Prelude.fromIntegral len)
                                                    Data.ProtoLens.parseMessage)
                                                Data.ProtoLens.Encoding.Bytes.<?>
                                                "transmit_timestamp"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"transmitTimestamp")
                                              y
                                              x)
                                           mutable'commands
                                           mutable'errors
                                           mutable'infos
                                           mutable'warnings
                                34 -> do !y <- (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                               Data.ProtoLens.Encoding.Bytes.getBytes
                                                                 (Prelude.fromIntegral len)
                                                   Data.ProtoLens.Encoding.Bytes.runEither
                                                     (case Data.Text.Encoding.decodeUtf8' value of
                                                          Prelude.Left err -> Prelude.Left
                                                                                (Prelude.show err)
                                                          Prelude.Right r -> Prelude.Right r))
                                                 Data.ProtoLens.Encoding.Bytes.<?> "errors"
                                         v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                (Data.ProtoLens.Encoding.Growing.append
                                                   mutable'errors
                                                   y)
                                         loop x mutable'commands v mutable'infos mutable'warnings
                                42 -> do !y <- (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                               Data.ProtoLens.Encoding.Bytes.getBytes
                                                                 (Prelude.fromIntegral len)
                                                   Data.ProtoLens.Encoding.Bytes.runEither
                                                     (case Data.Text.Encoding.decodeUtf8' value of
                                                          Prelude.Left err -> Prelude.Left
                                                                                (Prelude.show err)
                                                          Prelude.Right r -> Prelude.Right r))
                                                 Data.ProtoLens.Encoding.Bytes.<?> "warnings"
                                         v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                (Data.ProtoLens.Encoding.Growing.append
                                                   mutable'warnings
                                                   y)
                                         loop x mutable'commands mutable'errors mutable'infos v
                                50 -> do !y <- (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                               Data.ProtoLens.Encoding.Bytes.getBytes
                                                                 (Prelude.fromIntegral len)
                                                   Data.ProtoLens.Encoding.Bytes.runEither
                                                     (case Data.Text.Encoding.decodeUtf8' value of
                                                          Prelude.Left err -> Prelude.Left
                                                                                (Prelude.show err)
                                                          Prelude.Right r -> Prelude.Right r))
                                                 Data.ProtoLens.Encoding.Bytes.<?> "infos"
                                         v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                (Data.ProtoLens.Encoding.Growing.append
                                                   mutable'infos
                                                   y)
                                         loop x mutable'commands mutable'errors v mutable'warnings
                                wire -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                                   wire
                                           loop
                                             (Lens.Family2.over Data.ProtoLens.unknownFields
                                                (\ !t -> (:) y t)
                                                x)
                                             mutable'commands
                                             mutable'errors
                                             mutable'infos
                                             mutable'warnings
              in
              (do mutable'commands <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                        Data.ProtoLens.Encoding.Growing.new
                  mutable'errors <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                      Data.ProtoLens.Encoding.Growing.new
                  mutable'infos <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                     Data.ProtoLens.Encoding.Growing.new
                  mutable'warnings <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                        Data.ProtoLens.Encoding.Growing.new
                  loop Data.ProtoLens.defMessage mutable'commands mutable'errors
                    mutable'infos
                    mutable'warnings)
                Data.ProtoLens.Encoding.Bytes.<?> "ReportResponse"
        buildMessage
          = (\ _x ->
               (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                  (\ _v ->
                     (Data.ProtoLens.Encoding.Bytes.putVarInt 10) Data.Monoid.<>
                       (((\ bs ->
                            (Data.ProtoLens.Encoding.Bytes.putVarInt
                               (Prelude.fromIntegral (Data.ByteString.length bs)))
                              Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Prelude.. Data.ProtoLens.encodeMessage)
                         _v)
                  (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'commands")
                     _x))
                 Data.Monoid.<>
                 (case
                    Lens.Family2.view
                      (Data.ProtoLens.Field.field @"maybe'receiveTimestamp")
                      _x
                    of
                      (Prelude.Nothing) -> Data.Monoid.mempty
                      Prelude.Just _v -> (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                                           Data.Monoid.<>
                                           (((\ bs ->
                                                (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                   (Prelude.fromIntegral
                                                      (Data.ByteString.length bs)))
                                                  Data.Monoid.<>
                                                  Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                              Prelude.. Data.ProtoLens.encodeMessage)
                                             _v)
                   Data.Monoid.<>
                   (case
                      Lens.Family2.view
                        (Data.ProtoLens.Field.field @"maybe'transmitTimestamp")
                        _x
                      of
                        (Prelude.Nothing) -> Data.Monoid.mempty
                        Prelude.Just _v -> (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                                             Data.Monoid.<>
                                             (((\ bs ->
                                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                     (Prelude.fromIntegral
                                                        (Data.ByteString.length bs)))
                                                    Data.Monoid.<>
                                                    Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                                Prelude.. Data.ProtoLens.encodeMessage)
                                               _v)
                     Data.Monoid.<>
                     (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                        (\ _v ->
                           (Data.ProtoLens.Encoding.Bytes.putVarInt 34) Data.Monoid.<>
                             (((\ bs ->
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                    Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                Prelude.. Data.Text.Encoding.encodeUtf8)
                               _v)
                        (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'errors") _x))
                       Data.Monoid.<>
                       (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                          (\ _v ->
                             (Data.ProtoLens.Encoding.Bytes.putVarInt 42) Data.Monoid.<>
                               (((\ bs ->
                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                       (Prelude.fromIntegral (Data.ByteString.length bs)))
                                      Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                  Prelude.. Data.Text.Encoding.encodeUtf8)
                                 _v)
                          (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'warnings")
                             _x))
                         Data.Monoid.<>
                         (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                            (\ _v ->
                               (Data.ProtoLens.Encoding.Bytes.putVarInt 50) Data.Monoid.<>
                                 (((\ bs ->
                                      (Data.ProtoLens.Encoding.Bytes.putVarInt
                                         (Prelude.fromIntegral (Data.ByteString.length bs)))
                                        Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                    Prelude.. Data.Text.Encoding.encodeUtf8)
                                   _v)
                            (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'infos") _x))
                           Data.Monoid.<>
                           Data.ProtoLens.Encoding.Wire.buildFieldSet
                             (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData ReportResponse where
        rnf
          = (\ x__ ->
               Control.DeepSeq.deepseq (_ReportResponse'_unknownFields x__)
                 (Control.DeepSeq.deepseq (_ReportResponse'commands x__)
                    (Control.DeepSeq.deepseq (_ReportResponse'receiveTimestamp x__)
                       (Control.DeepSeq.deepseq (_ReportResponse'transmitTimestamp x__)
                          (Control.DeepSeq.deepseq (_ReportResponse'errors x__)
                             (Control.DeepSeq.deepseq (_ReportResponse'warnings x__)
                                (Control.DeepSeq.deepseq (_ReportResponse'infos x__) (()))))))))
{- | Fields :

    * 'Proto.Collector_Fields.reporterId' @:: Lens' Reporter Data.Word.Word64@
    * 'Proto.Collector_Fields.tags' @:: Lens' Reporter [KeyValue]@
    * 'Proto.Collector_Fields.vec'tags' @:: Lens' Reporter (Data.Vector.Vector KeyValue)@
 -}
data Reporter = Reporter{_Reporter'reporterId :: !Data.Word.Word64,
                         _Reporter'tags :: !(Data.Vector.Vector KeyValue),
                         _Reporter'_unknownFields :: !Data.ProtoLens.FieldSet}
                  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Reporter where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Reporter "reporterId"
           (Data.Word.Word64)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _Reporter'reporterId
               (\ x__ y__ -> x__{_Reporter'reporterId = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField Reporter "tags" ([KeyValue])
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _Reporter'tags
               (\ x__ y__ -> x__{_Reporter'tags = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens Data.Vector.Generic.toList
                (\ _ y__ -> Data.Vector.Generic.fromList y__)
instance Data.ProtoLens.Field.HasField Reporter "vec'tags"
           (Data.Vector.Vector KeyValue)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _Reporter'tags
               (\ x__ y__ -> x__{_Reporter'tags = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Message Reporter where
        messageName _ = Data.Text.pack "lightstep.collector.Reporter"
        fieldsByTag
          = let reporterId__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "reporter_id"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"reporterId"))
                      :: Data.ProtoLens.FieldDescriptor Reporter
                tags__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "tags"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor KeyValue)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Data.ProtoLens.Field.field @"tags"))
                      :: Data.ProtoLens.FieldDescriptor Reporter
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, reporterId__field_descriptor),
                 (Data.ProtoLens.Tag 4, tags__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _Reporter'_unknownFields
              (\ x__ y__ -> x__{_Reporter'_unknownFields = y__})
        defMessage
          = Reporter{_Reporter'reporterId = Data.ProtoLens.fieldDefault,
                     _Reporter'tags = Data.Vector.Generic.empty,
                     _Reporter'_unknownFields = ([])}
        parseMessage
          = let loop ::
                     Reporter ->
                       Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector
                         Data.ProtoLens.Encoding.Growing.RealWorld
                         KeyValue
                         -> Data.ProtoLens.Encoding.Bytes.Parser Reporter
                loop x mutable'tags
                  = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
                       if end then
                         do frozen'tags <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                             (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                mutable'tags)
                            let missing = [] in
                              if Prelude.null missing then Prelude.return () else
                                Prelude.fail
                                  (("Missing required fields: ") Prelude.++
                                     Prelude.show (missing :: ([Prelude.String])))
                            Prelude.return
                              (Lens.Family2.over Data.ProtoLens.unknownFields
                                 (\ !t -> Prelude.reverse t)
                                 (Lens.Family2.set (Data.ProtoLens.Field.field @"vec'tags")
                                    frozen'tags
                                    x))
                         else
                         do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                            case tag of
                                8 -> do y <- (Data.ProtoLens.Encoding.Bytes.getVarInt)
                                               Data.ProtoLens.Encoding.Bytes.<?> "reporter_id"
                                        loop
                                          (Lens.Family2.set
                                             (Data.ProtoLens.Field.field @"reporterId")
                                             y
                                             x)
                                          mutable'tags
                                34 -> do !y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                   Data.ProtoLens.Encoding.Bytes.isolate
                                                     (Prelude.fromIntegral len)
                                                     Data.ProtoLens.parseMessage)
                                                 Data.ProtoLens.Encoding.Bytes.<?> "tags"
                                         v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                (Data.ProtoLens.Encoding.Growing.append mutable'tags
                                                   y)
                                         loop x v
                                wire -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                                   wire
                                           loop
                                             (Lens.Family2.over Data.ProtoLens.unknownFields
                                                (\ !t -> (:) y t)
                                                x)
                                             mutable'tags
              in
              (do mutable'tags <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                    Data.ProtoLens.Encoding.Growing.new
                  loop Data.ProtoLens.defMessage mutable'tags)
                Data.ProtoLens.Encoding.Bytes.<?> "Reporter"
        buildMessage
          = (\ _x ->
               (let _v
                      = Lens.Family2.view (Data.ProtoLens.Field.field @"reporterId") _x
                  in
                  if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty else
                    (Data.ProtoLens.Encoding.Bytes.putVarInt 8) Data.Monoid.<>
                      Data.ProtoLens.Encoding.Bytes.putVarInt _v)
                 Data.Monoid.<>
                 (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                    (\ _v ->
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 34) Data.Monoid.<>
                         (((\ bs ->
                              (Data.ProtoLens.Encoding.Bytes.putVarInt
                                 (Prelude.fromIntegral (Data.ByteString.length bs)))
                                Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            Prelude.. Data.ProtoLens.encodeMessage)
                           _v)
                    (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'tags") _x))
                   Data.Monoid.<>
                   Data.ProtoLens.Encoding.Wire.buildFieldSet
                     (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData Reporter where
        rnf
          = (\ x__ ->
               Control.DeepSeq.deepseq (_Reporter'_unknownFields x__)
                 (Control.DeepSeq.deepseq (_Reporter'reporterId x__)
                    (Control.DeepSeq.deepseq (_Reporter'tags x__) (()))))
{- | Fields :

    * 'Proto.Collector_Fields.spanContext' @:: Lens' Span SpanContext@
    * 'Proto.Collector_Fields.maybe'spanContext' @:: Lens' Span (Prelude.Maybe SpanContext)@
    * 'Proto.Collector_Fields.operationName' @:: Lens' Span Data.Text.Text@
    * 'Proto.Collector_Fields.references' @:: Lens' Span [Reference]@
    * 'Proto.Collector_Fields.vec'references' @:: Lens' Span (Data.Vector.Vector Reference)@
    * 'Proto.Collector_Fields.startTimestamp' @:: Lens' Span Proto.Google.Protobuf.Timestamp.Timestamp@
    * 'Proto.Collector_Fields.maybe'startTimestamp' @:: Lens' Span
  (Prelude.Maybe Proto.Google.Protobuf.Timestamp.Timestamp)@
    * 'Proto.Collector_Fields.durationMicros' @:: Lens' Span Data.Word.Word64@
    * 'Proto.Collector_Fields.tags' @:: Lens' Span [KeyValue]@
    * 'Proto.Collector_Fields.vec'tags' @:: Lens' Span (Data.Vector.Vector KeyValue)@
    * 'Proto.Collector_Fields.logs' @:: Lens' Span [Log]@
    * 'Proto.Collector_Fields.vec'logs' @:: Lens' Span (Data.Vector.Vector Log)@
 -}
data Span = Span{_Span'spanContext :: !(Prelude.Maybe SpanContext),
                 _Span'operationName :: !Data.Text.Text,
                 _Span'references :: !(Data.Vector.Vector Reference),
                 _Span'startTimestamp ::
                 !(Prelude.Maybe Proto.Google.Protobuf.Timestamp.Timestamp),
                 _Span'durationMicros :: !Data.Word.Word64,
                 _Span'tags :: !(Data.Vector.Vector KeyValue),
                 _Span'logs :: !(Data.Vector.Vector Log),
                 _Span'_unknownFields :: !Data.ProtoLens.FieldSet}
              deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Span where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Span "spanContext"
           (SpanContext)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _Span'spanContext
               (\ x__ y__ -> x__{_Span'spanContext = y__}))
              Prelude.. Data.ProtoLens.maybeLens Data.ProtoLens.defMessage
instance Data.ProtoLens.Field.HasField Span "maybe'spanContext"
           (Prelude.Maybe SpanContext)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _Span'spanContext
               (\ x__ y__ -> x__{_Span'spanContext = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField Span "operationName"
           (Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _Span'operationName
               (\ x__ y__ -> x__{_Span'operationName = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField Span "references"
           ([Reference])
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _Span'references
               (\ x__ y__ -> x__{_Span'references = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens Data.Vector.Generic.toList
                (\ _ y__ -> Data.Vector.Generic.fromList y__)
instance Data.ProtoLens.Field.HasField Span "vec'references"
           (Data.Vector.Vector Reference)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _Span'references
               (\ x__ y__ -> x__{_Span'references = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField Span "startTimestamp"
           (Proto.Google.Protobuf.Timestamp.Timestamp)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _Span'startTimestamp
               (\ x__ y__ -> x__{_Span'startTimestamp = y__}))
              Prelude.. Data.ProtoLens.maybeLens Data.ProtoLens.defMessage
instance Data.ProtoLens.Field.HasField Span "maybe'startTimestamp"
           (Prelude.Maybe Proto.Google.Protobuf.Timestamp.Timestamp)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _Span'startTimestamp
               (\ x__ y__ -> x__{_Span'startTimestamp = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField Span "durationMicros"
           (Data.Word.Word64)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _Span'durationMicros
               (\ x__ y__ -> x__{_Span'durationMicros = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField Span "tags" ([KeyValue])
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _Span'tags
               (\ x__ y__ -> x__{_Span'tags = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens Data.Vector.Generic.toList
                (\ _ y__ -> Data.Vector.Generic.fromList y__)
instance Data.ProtoLens.Field.HasField Span "vec'tags"
           (Data.Vector.Vector KeyValue)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _Span'tags
               (\ x__ y__ -> x__{_Span'tags = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField Span "logs" ([Log]) where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _Span'logs
               (\ x__ y__ -> x__{_Span'logs = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens Data.Vector.Generic.toList
                (\ _ y__ -> Data.Vector.Generic.fromList y__)
instance Data.ProtoLens.Field.HasField Span "vec'logs"
           (Data.Vector.Vector Log)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _Span'logs
               (\ x__ y__ -> x__{_Span'logs = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Message Span where
        messageName _ = Data.Text.pack "lightstep.collector.Span"
        fieldsByTag
          = let spanContext__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "span_context"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor SpanContext)
                      (Data.ProtoLens.OptionalField
                         (Data.ProtoLens.Field.field @"maybe'spanContext"))
                      :: Data.ProtoLens.FieldDescriptor Span
                operationName__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "operation_name"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"operationName"))
                      :: Data.ProtoLens.FieldDescriptor Span
                references__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "references"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor Reference)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Data.ProtoLens.Field.field @"references"))
                      :: Data.ProtoLens.FieldDescriptor Span
                startTimestamp__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "start_timestamp"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor
                           Proto.Google.Protobuf.Timestamp.Timestamp)
                      (Data.ProtoLens.OptionalField
                         (Data.ProtoLens.Field.field @"maybe'startTimestamp"))
                      :: Data.ProtoLens.FieldDescriptor Span
                durationMicros__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "duration_micros"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"durationMicros"))
                      :: Data.ProtoLens.FieldDescriptor Span
                tags__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "tags"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor KeyValue)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Data.ProtoLens.Field.field @"tags"))
                      :: Data.ProtoLens.FieldDescriptor Span
                logs__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "logs"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor Log)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Data.ProtoLens.Field.field @"logs"))
                      :: Data.ProtoLens.FieldDescriptor Span
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, spanContext__field_descriptor),
                 (Data.ProtoLens.Tag 2, operationName__field_descriptor),
                 (Data.ProtoLens.Tag 3, references__field_descriptor),
                 (Data.ProtoLens.Tag 4, startTimestamp__field_descriptor),
                 (Data.ProtoLens.Tag 5, durationMicros__field_descriptor),
                 (Data.ProtoLens.Tag 6, tags__field_descriptor),
                 (Data.ProtoLens.Tag 7, logs__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _Span'_unknownFields
              (\ x__ y__ -> x__{_Span'_unknownFields = y__})
        defMessage
          = Span{_Span'spanContext = Prelude.Nothing,
                 _Span'operationName = Data.ProtoLens.fieldDefault,
                 _Span'references = Data.Vector.Generic.empty,
                 _Span'startTimestamp = Prelude.Nothing,
                 _Span'durationMicros = Data.ProtoLens.fieldDefault,
                 _Span'tags = Data.Vector.Generic.empty,
                 _Span'logs = Data.Vector.Generic.empty,
                 _Span'_unknownFields = ([])}
        parseMessage
          = let loop ::
                     Span ->
                       Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector
                         Data.ProtoLens.Encoding.Growing.RealWorld
                         Log
                         ->
                         Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector
                           Data.ProtoLens.Encoding.Growing.RealWorld
                           Reference
                           ->
                           Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector
                             Data.ProtoLens.Encoding.Growing.RealWorld
                             KeyValue
                             -> Data.ProtoLens.Encoding.Bytes.Parser Span
                loop x mutable'logs mutable'references mutable'tags
                  = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
                       if end then
                         do frozen'logs <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                             (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                mutable'logs)
                            frozen'references <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                   (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                      mutable'references)
                            frozen'tags <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                             (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                mutable'tags)
                            let missing = [] in
                              if Prelude.null missing then Prelude.return () else
                                Prelude.fail
                                  (("Missing required fields: ") Prelude.++
                                     Prelude.show (missing :: ([Prelude.String])))
                            Prelude.return
                              (Lens.Family2.over Data.ProtoLens.unknownFields
                                 (\ !t -> Prelude.reverse t)
                                 (Lens.Family2.set (Data.ProtoLens.Field.field @"vec'logs")
                                    frozen'logs
                                    (Lens.Family2.set (Data.ProtoLens.Field.field @"vec'references")
                                       frozen'references
                                       (Lens.Family2.set (Data.ProtoLens.Field.field @"vec'tags")
                                          frozen'tags
                                          x))))
                         else
                         do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                            case tag of
                                10 -> do y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                  Data.ProtoLens.Encoding.Bytes.isolate
                                                    (Prelude.fromIntegral len)
                                                    Data.ProtoLens.parseMessage)
                                                Data.ProtoLens.Encoding.Bytes.<?> "span_context"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"spanContext")
                                              y
                                              x)
                                           mutable'logs
                                           mutable'references
                                           mutable'tags
                                18 -> do y <- (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                              Data.ProtoLens.Encoding.Bytes.getBytes
                                                                (Prelude.fromIntegral len)
                                                  Data.ProtoLens.Encoding.Bytes.runEither
                                                    (case Data.Text.Encoding.decodeUtf8' value of
                                                         Prelude.Left err -> Prelude.Left
                                                                               (Prelude.show err)
                                                         Prelude.Right r -> Prelude.Right r))
                                                Data.ProtoLens.Encoding.Bytes.<?> "operation_name"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"operationName")
                                              y
                                              x)
                                           mutable'logs
                                           mutable'references
                                           mutable'tags
                                26 -> do !y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                   Data.ProtoLens.Encoding.Bytes.isolate
                                                     (Prelude.fromIntegral len)
                                                     Data.ProtoLens.parseMessage)
                                                 Data.ProtoLens.Encoding.Bytes.<?> "references"
                                         v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                (Data.ProtoLens.Encoding.Growing.append
                                                   mutable'references
                                                   y)
                                         loop x mutable'logs v mutable'tags
                                34 -> do y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                  Data.ProtoLens.Encoding.Bytes.isolate
                                                    (Prelude.fromIntegral len)
                                                    Data.ProtoLens.parseMessage)
                                                Data.ProtoLens.Encoding.Bytes.<?> "start_timestamp"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"startTimestamp")
                                              y
                                              x)
                                           mutable'logs
                                           mutable'references
                                           mutable'tags
                                40 -> do y <- (Data.ProtoLens.Encoding.Bytes.getVarInt)
                                                Data.ProtoLens.Encoding.Bytes.<?> "duration_micros"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"durationMicros")
                                              y
                                              x)
                                           mutable'logs
                                           mutable'references
                                           mutable'tags
                                50 -> do !y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                   Data.ProtoLens.Encoding.Bytes.isolate
                                                     (Prelude.fromIntegral len)
                                                     Data.ProtoLens.parseMessage)
                                                 Data.ProtoLens.Encoding.Bytes.<?> "tags"
                                         v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                (Data.ProtoLens.Encoding.Growing.append mutable'tags
                                                   y)
                                         loop x mutable'logs mutable'references v
                                58 -> do !y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                   Data.ProtoLens.Encoding.Bytes.isolate
                                                     (Prelude.fromIntegral len)
                                                     Data.ProtoLens.parseMessage)
                                                 Data.ProtoLens.Encoding.Bytes.<?> "logs"
                                         v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                (Data.ProtoLens.Encoding.Growing.append mutable'logs
                                                   y)
                                         loop x v mutable'references mutable'tags
                                wire -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                                   wire
                                           loop
                                             (Lens.Family2.over Data.ProtoLens.unknownFields
                                                (\ !t -> (:) y t)
                                                x)
                                             mutable'logs
                                             mutable'references
                                             mutable'tags
              in
              (do mutable'logs <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                    Data.ProtoLens.Encoding.Growing.new
                  mutable'references <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                          Data.ProtoLens.Encoding.Growing.new
                  mutable'tags <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                    Data.ProtoLens.Encoding.Growing.new
                  loop Data.ProtoLens.defMessage mutable'logs mutable'references
                    mutable'tags)
                Data.ProtoLens.Encoding.Bytes.<?> "Span"
        buildMessage
          = (\ _x ->
               (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'spanContext")
                    _x
                  of
                    (Prelude.Nothing) -> Data.Monoid.mempty
                    Prelude.Just _v -> (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                                         Data.Monoid.<>
                                         (((\ bs ->
                                              (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                 (Prelude.fromIntegral (Data.ByteString.length bs)))
                                                Data.Monoid.<>
                                                Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                            Prelude.. Data.ProtoLens.encodeMessage)
                                           _v)
                 Data.Monoid.<>
                 (let _v
                        = Lens.Family2.view (Data.ProtoLens.Field.field @"operationName")
                            _x
                    in
                    if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                      Data.Monoid.mempty else
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 18) Data.Monoid.<>
                        (((\ bs ->
                             (Data.ProtoLens.Encoding.Bytes.putVarInt
                                (Prelude.fromIntegral (Data.ByteString.length bs)))
                               Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                           Prelude.. Data.Text.Encoding.encodeUtf8)
                          _v)
                   Data.Monoid.<>
                   (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                      (\ _v ->
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 26) Data.Monoid.<>
                           (((\ bs ->
                                (Data.ProtoLens.Encoding.Bytes.putVarInt
                                   (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                              Prelude.. Data.ProtoLens.encodeMessage)
                             _v)
                      (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'references")
                         _x))
                     Data.Monoid.<>
                     (case
                        Lens.Family2.view
                          (Data.ProtoLens.Field.field @"maybe'startTimestamp")
                          _x
                        of
                          (Prelude.Nothing) -> Data.Monoid.mempty
                          Prelude.Just _v -> (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                                               Data.Monoid.<>
                                               (((\ bs ->
                                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                       (Prelude.fromIntegral
                                                          (Data.ByteString.length bs)))
                                                      Data.Monoid.<>
                                                      Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                                  Prelude.. Data.ProtoLens.encodeMessage)
                                                 _v)
                       Data.Monoid.<>
                       (let _v
                              = Lens.Family2.view (Data.ProtoLens.Field.field @"durationMicros")
                                  _x
                          in
                          if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                            Data.Monoid.mempty else
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 40) Data.Monoid.<>
                              Data.ProtoLens.Encoding.Bytes.putVarInt _v)
                         Data.Monoid.<>
                         (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                            (\ _v ->
                               (Data.ProtoLens.Encoding.Bytes.putVarInt 50) Data.Monoid.<>
                                 (((\ bs ->
                                      (Data.ProtoLens.Encoding.Bytes.putVarInt
                                         (Prelude.fromIntegral (Data.ByteString.length bs)))
                                        Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                    Prelude.. Data.ProtoLens.encodeMessage)
                                   _v)
                            (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'tags") _x))
                           Data.Monoid.<>
                           (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                              (\ _v ->
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt 58) Data.Monoid.<>
                                   (((\ bs ->
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                           (Prelude.fromIntegral (Data.ByteString.length bs)))
                                          Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                      Prelude.. Data.ProtoLens.encodeMessage)
                                     _v)
                              (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'logs") _x))
                             Data.Monoid.<>
                             Data.ProtoLens.Encoding.Wire.buildFieldSet
                               (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData Span where
        rnf
          = (\ x__ ->
               Control.DeepSeq.deepseq (_Span'_unknownFields x__)
                 (Control.DeepSeq.deepseq (_Span'spanContext x__)
                    (Control.DeepSeq.deepseq (_Span'operationName x__)
                       (Control.DeepSeq.deepseq (_Span'references x__)
                          (Control.DeepSeq.deepseq (_Span'startTimestamp x__)
                             (Control.DeepSeq.deepseq (_Span'durationMicros x__)
                                (Control.DeepSeq.deepseq (_Span'tags x__)
                                   (Control.DeepSeq.deepseq (_Span'logs x__) (())))))))))
{- | Fields :

    * 'Proto.Collector_Fields.traceId' @:: Lens' SpanContext Data.Word.Word64@
    * 'Proto.Collector_Fields.spanId' @:: Lens' SpanContext Data.Word.Word64@
    * 'Proto.Collector_Fields.baggage' @:: Lens' SpanContext (Data.Map.Map Data.Text.Text Data.Text.Text)@
 -}
data SpanContext = SpanContext{_SpanContext'traceId ::
                               !Data.Word.Word64,
                               _SpanContext'spanId :: !Data.Word.Word64,
                               _SpanContext'baggage ::
                               !(Data.Map.Map Data.Text.Text Data.Text.Text),
                               _SpanContext'_unknownFields :: !Data.ProtoLens.FieldSet}
                     deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show SpanContext where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField SpanContext "traceId"
           (Data.Word.Word64)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _SpanContext'traceId
               (\ x__ y__ -> x__{_SpanContext'traceId = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField SpanContext "spanId"
           (Data.Word.Word64)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _SpanContext'spanId
               (\ x__ y__ -> x__{_SpanContext'spanId = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField SpanContext "baggage"
           (Data.Map.Map Data.Text.Text Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _SpanContext'baggage
               (\ x__ y__ -> x__{_SpanContext'baggage = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Message SpanContext where
        messageName _ = Data.Text.pack "lightstep.collector.SpanContext"
        fieldsByTag
          = let traceId__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "trace_id"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"traceId"))
                      :: Data.ProtoLens.FieldDescriptor SpanContext
                spanId__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "span_id"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"spanId"))
                      :: Data.ProtoLens.FieldDescriptor SpanContext
                baggage__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "baggage"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor SpanContext'BaggageEntry)
                      (Data.ProtoLens.MapField (Data.ProtoLens.Field.field @"key")
                         (Data.ProtoLens.Field.field @"value")
                         (Data.ProtoLens.Field.field @"baggage"))
                      :: Data.ProtoLens.FieldDescriptor SpanContext
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, traceId__field_descriptor),
                 (Data.ProtoLens.Tag 2, spanId__field_descriptor),
                 (Data.ProtoLens.Tag 3, baggage__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _SpanContext'_unknownFields
              (\ x__ y__ -> x__{_SpanContext'_unknownFields = y__})
        defMessage
          = SpanContext{_SpanContext'traceId = Data.ProtoLens.fieldDefault,
                        _SpanContext'spanId = Data.ProtoLens.fieldDefault,
                        _SpanContext'baggage = Data.Map.empty,
                        _SpanContext'_unknownFields = ([])}
        parseMessage
          = let loop ::
                     SpanContext -> Data.ProtoLens.Encoding.Bytes.Parser SpanContext
                loop x
                  = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
                       if end then
                         do let missing = [] in
                              if Prelude.null missing then Prelude.return () else
                                Prelude.fail
                                  (("Missing required fields: ") Prelude.++
                                     Prelude.show (missing :: ([Prelude.String])))
                            Prelude.return
                              (Lens.Family2.over Data.ProtoLens.unknownFields
                                 (\ !t -> Prelude.reverse t)
                                 x)
                         else
                         do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                            case tag of
                                8 -> do y <- (Data.ProtoLens.Encoding.Bytes.getVarInt)
                                               Data.ProtoLens.Encoding.Bytes.<?> "trace_id"
                                        loop
                                          (Lens.Family2.set (Data.ProtoLens.Field.field @"traceId")
                                             y
                                             x)
                                16 -> do y <- (Data.ProtoLens.Encoding.Bytes.getVarInt)
                                                Data.ProtoLens.Encoding.Bytes.<?> "span_id"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"spanId")
                                              y
                                              x)
                                26 -> do !(entry ::
                                             SpanContext'BaggageEntry) <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                              Data.ProtoLens.Encoding.Bytes.isolate
                                                                                (Prelude.fromIntegral
                                                                                   len)
                                                                                Data.ProtoLens.parseMessage)
                                                                            Data.ProtoLens.Encoding.Bytes.<?>
                                                                            "baggage"
                                         let key
                                               = Lens.Family2.view
                                                   (Data.ProtoLens.Field.field @"key")
                                                   entry
                                             value
                                               = Lens.Family2.view
                                                   (Data.ProtoLens.Field.field @"value")
                                                   entry
                                           in
                                           loop
                                             (Lens.Family2.over
                                                (Data.ProtoLens.Field.field @"baggage")
                                                (\ !t -> Data.Map.insert key value t)
                                                x)
                                wire -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                                   wire
                                           loop
                                             (Lens.Family2.over Data.ProtoLens.unknownFields
                                                (\ !t -> (:) y t)
                                                x)
              in
              (do loop Data.ProtoLens.defMessage)
                Data.ProtoLens.Encoding.Bytes.<?> "SpanContext"
        buildMessage
          = (\ _x ->
               (let _v
                      = Lens.Family2.view (Data.ProtoLens.Field.field @"traceId") _x
                  in
                  if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty else
                    (Data.ProtoLens.Encoding.Bytes.putVarInt 8) Data.Monoid.<>
                      Data.ProtoLens.Encoding.Bytes.putVarInt _v)
                 Data.Monoid.<>
                 (let _v
                        = Lens.Family2.view (Data.ProtoLens.Field.field @"spanId") _x
                    in
                    if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                      Data.Monoid.mempty else
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 16) Data.Monoid.<>
                        Data.ProtoLens.Encoding.Bytes.putVarInt _v)
                   Data.Monoid.<>
                   (Data.Monoid.mconcat
                      (Prelude.map
                         (\ _v ->
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 26) Data.Monoid.<>
                              (((\ bs ->
                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                      (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                 Prelude.. Data.ProtoLens.encodeMessage)
                                (Lens.Family2.set (Data.ProtoLens.Field.field @"key")
                                   (Prelude.fst _v)
                                   (Lens.Family2.set (Data.ProtoLens.Field.field @"value")
                                      (Prelude.snd _v)
                                      (Data.ProtoLens.defMessage :: SpanContext'BaggageEntry))))
                         (Data.Map.toList
                            (Lens.Family2.view (Data.ProtoLens.Field.field @"baggage") _x))))
                     Data.Monoid.<>
                     Data.ProtoLens.Encoding.Wire.buildFieldSet
                       (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData SpanContext where
        rnf
          = (\ x__ ->
               Control.DeepSeq.deepseq (_SpanContext'_unknownFields x__)
                 (Control.DeepSeq.deepseq (_SpanContext'traceId x__)
                    (Control.DeepSeq.deepseq (_SpanContext'spanId x__)
                       (Control.DeepSeq.deepseq (_SpanContext'baggage x__) (())))))
{- | Fields :

    * 'Proto.Collector_Fields.key' @:: Lens' SpanContext'BaggageEntry Data.Text.Text@
    * 'Proto.Collector_Fields.value' @:: Lens' SpanContext'BaggageEntry Data.Text.Text@
 -}
data SpanContext'BaggageEntry = SpanContext'BaggageEntry{_SpanContext'BaggageEntry'key
                                                         :: !Data.Text.Text,
                                                         _SpanContext'BaggageEntry'value ::
                                                         !Data.Text.Text,
                                                         _SpanContext'BaggageEntry'_unknownFields ::
                                                         !Data.ProtoLens.FieldSet}
                                  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show SpanContext'BaggageEntry where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField SpanContext'BaggageEntry
           "key"
           (Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _SpanContext'BaggageEntry'key
               (\ x__ y__ -> x__{_SpanContext'BaggageEntry'key = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField SpanContext'BaggageEntry
           "value"
           (Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _SpanContext'BaggageEntry'value
               (\ x__ y__ -> x__{_SpanContext'BaggageEntry'value = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Message SpanContext'BaggageEntry where
        messageName _
          = Data.Text.pack "lightstep.collector.SpanContext.BaggageEntry"
        fieldsByTag
          = let key__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "key"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"key"))
                      :: Data.ProtoLens.FieldDescriptor SpanContext'BaggageEntry
                value__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "value"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"value"))
                      :: Data.ProtoLens.FieldDescriptor SpanContext'BaggageEntry
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, key__field_descriptor),
                 (Data.ProtoLens.Tag 2, value__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens
              _SpanContext'BaggageEntry'_unknownFields
              (\ x__ y__ -> x__{_SpanContext'BaggageEntry'_unknownFields = y__})
        defMessage
          = SpanContext'BaggageEntry{_SpanContext'BaggageEntry'key =
                                       Data.ProtoLens.fieldDefault,
                                     _SpanContext'BaggageEntry'value = Data.ProtoLens.fieldDefault,
                                     _SpanContext'BaggageEntry'_unknownFields = ([])}
        parseMessage
          = let loop ::
                     SpanContext'BaggageEntry ->
                       Data.ProtoLens.Encoding.Bytes.Parser SpanContext'BaggageEntry
                loop x
                  = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
                       if end then
                         do let missing = [] in
                              if Prelude.null missing then Prelude.return () else
                                Prelude.fail
                                  (("Missing required fields: ") Prelude.++
                                     Prelude.show (missing :: ([Prelude.String])))
                            Prelude.return
                              (Lens.Family2.over Data.ProtoLens.unknownFields
                                 (\ !t -> Prelude.reverse t)
                                 x)
                         else
                         do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                            case tag of
                                10 -> do y <- (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                              Data.ProtoLens.Encoding.Bytes.getBytes
                                                                (Prelude.fromIntegral len)
                                                  Data.ProtoLens.Encoding.Bytes.runEither
                                                    (case Data.Text.Encoding.decodeUtf8' value of
                                                         Prelude.Left err -> Prelude.Left
                                                                               (Prelude.show err)
                                                         Prelude.Right r -> Prelude.Right r))
                                                Data.ProtoLens.Encoding.Bytes.<?> "key"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"key") y
                                              x)
                                18 -> do y <- (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                              Data.ProtoLens.Encoding.Bytes.getBytes
                                                                (Prelude.fromIntegral len)
                                                  Data.ProtoLens.Encoding.Bytes.runEither
                                                    (case Data.Text.Encoding.decodeUtf8' value of
                                                         Prelude.Left err -> Prelude.Left
                                                                               (Prelude.show err)
                                                         Prelude.Right r -> Prelude.Right r))
                                                Data.ProtoLens.Encoding.Bytes.<?> "value"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"value") y
                                              x)
                                wire -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                                   wire
                                           loop
                                             (Lens.Family2.over Data.ProtoLens.unknownFields
                                                (\ !t -> (:) y t)
                                                x)
              in
              (do loop Data.ProtoLens.defMessage)
                Data.ProtoLens.Encoding.Bytes.<?> "BaggageEntry"
        buildMessage
          = (\ _x ->
               (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"key") _x
                  in
                  if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty else
                    (Data.ProtoLens.Encoding.Bytes.putVarInt 10) Data.Monoid.<>
                      (((\ bs ->
                           (Data.ProtoLens.Encoding.Bytes.putVarInt
                              (Prelude.fromIntegral (Data.ByteString.length bs)))
                             Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Prelude.. Data.Text.Encoding.encodeUtf8)
                        _v)
                 Data.Monoid.<>
                 (let _v
                        = Lens.Family2.view (Data.ProtoLens.Field.field @"value") _x
                    in
                    if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                      Data.Monoid.mempty else
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 18) Data.Monoid.<>
                        (((\ bs ->
                             (Data.ProtoLens.Encoding.Bytes.putVarInt
                                (Prelude.fromIntegral (Data.ByteString.length bs)))
                               Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                           Prelude.. Data.Text.Encoding.encodeUtf8)
                          _v)
                   Data.Monoid.<>
                   Data.ProtoLens.Encoding.Wire.buildFieldSet
                     (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData SpanContext'BaggageEntry where
        rnf
          = (\ x__ ->
               Control.DeepSeq.deepseq
                 (_SpanContext'BaggageEntry'_unknownFields x__)
                 (Control.DeepSeq.deepseq (_SpanContext'BaggageEntry'key x__)
                    (Control.DeepSeq.deepseq (_SpanContext'BaggageEntry'value x__)
                       (()))))
data CollectorService = CollectorService{}
                          deriving ()
instance Data.ProtoLens.Service.Types.Service CollectorService
         where
        type ServiceName CollectorService = "CollectorService"
        type ServicePackage CollectorService = "lightstep.collector"
        type ServiceMethods CollectorService = '["report"]
instance Data.ProtoLens.Service.Types.HasMethodImpl
           CollectorService
           "report"
         where
        type MethodName CollectorService "report" = "Report"
        type MethodInput CollectorService "report" = ReportRequest
        type MethodOutput CollectorService "report" = ReportResponse
        type MethodStreamingType CollectorService "report" =
             'Data.ProtoLens.Service.Types.NonStreaming