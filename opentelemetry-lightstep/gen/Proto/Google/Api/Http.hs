{- This file was auto-generated from google/api/http.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies,
  UndecidableInstances, GeneralizedNewtypeDeriving,
  MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
  PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds,
  BangPatterns, TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports#-}
module Proto.Google.Api.Http
       (CustomHttpPattern(), Http(), HttpRule(), HttpRule'Pattern(..),
        _HttpRule'Get, _HttpRule'Put, _HttpRule'Post, _HttpRule'Delete,
        _HttpRule'Patch, _HttpRule'Custom)
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

{- | Fields :

    * 'Proto.Google.Api.Http_Fields.kind' @:: Lens' CustomHttpPattern Data.Text.Text@
    * 'Proto.Google.Api.Http_Fields.path' @:: Lens' CustomHttpPattern Data.Text.Text@
 -}
data CustomHttpPattern = CustomHttpPattern{_CustomHttpPattern'kind
                                           :: !Data.Text.Text,
                                           _CustomHttpPattern'path :: !Data.Text.Text,
                                           _CustomHttpPattern'_unknownFields ::
                                           !Data.ProtoLens.FieldSet}
                           deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show CustomHttpPattern where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField CustomHttpPattern "kind"
           (Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _CustomHttpPattern'kind
               (\ x__ y__ -> x__{_CustomHttpPattern'kind = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField CustomHttpPattern "path"
           (Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _CustomHttpPattern'path
               (\ x__ y__ -> x__{_CustomHttpPattern'path = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Message CustomHttpPattern where
        messageName _ = Data.Text.pack "google.api.CustomHttpPattern"
        fieldsByTag
          = let kind__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "kind"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"kind"))
                      :: Data.ProtoLens.FieldDescriptor CustomHttpPattern
                path__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "path"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"path"))
                      :: Data.ProtoLens.FieldDescriptor CustomHttpPattern
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, kind__field_descriptor),
                 (Data.ProtoLens.Tag 2, path__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _CustomHttpPattern'_unknownFields
              (\ x__ y__ -> x__{_CustomHttpPattern'_unknownFields = y__})
        defMessage
          = CustomHttpPattern{_CustomHttpPattern'kind =
                                Data.ProtoLens.fieldDefault,
                              _CustomHttpPattern'path = Data.ProtoLens.fieldDefault,
                              _CustomHttpPattern'_unknownFields = ([])}
        parseMessage
          = let loop ::
                     CustomHttpPattern ->
                       Data.ProtoLens.Encoding.Bytes.Parser CustomHttpPattern
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
                                                Data.ProtoLens.Encoding.Bytes.<?> "kind"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"kind") y
                                              x)
                                18 -> do y <- (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                              Data.ProtoLens.Encoding.Bytes.getBytes
                                                                (Prelude.fromIntegral len)
                                                  Data.ProtoLens.Encoding.Bytes.runEither
                                                    (case Data.Text.Encoding.decodeUtf8' value of
                                                         Prelude.Left err -> Prelude.Left
                                                                               (Prelude.show err)
                                                         Prelude.Right r -> Prelude.Right r))
                                                Data.ProtoLens.Encoding.Bytes.<?> "path"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"path") y
                                              x)
                                wire -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                                   wire
                                           loop
                                             (Lens.Family2.over Data.ProtoLens.unknownFields
                                                (\ !t -> (:) y t)
                                                x)
              in
              (do loop Data.ProtoLens.defMessage)
                Data.ProtoLens.Encoding.Bytes.<?> "CustomHttpPattern"
        buildMessage
          = (\ _x ->
               (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"kind") _x
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
                 (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"path") _x
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
instance Control.DeepSeq.NFData CustomHttpPattern where
        rnf
          = (\ x__ ->
               Control.DeepSeq.deepseq (_CustomHttpPattern'_unknownFields x__)
                 (Control.DeepSeq.deepseq (_CustomHttpPattern'kind x__)
                    (Control.DeepSeq.deepseq (_CustomHttpPattern'path x__) (()))))
{- | Fields :

    * 'Proto.Google.Api.Http_Fields.rules' @:: Lens' Http [HttpRule]@
    * 'Proto.Google.Api.Http_Fields.vec'rules' @:: Lens' Http (Data.Vector.Vector HttpRule)@
 -}
data Http = Http{_Http'rules :: !(Data.Vector.Vector HttpRule),
                 _Http'_unknownFields :: !Data.ProtoLens.FieldSet}
              deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Http where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Http "rules" ([HttpRule])
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _Http'rules
               (\ x__ y__ -> x__{_Http'rules = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens Data.Vector.Generic.toList
                (\ _ y__ -> Data.Vector.Generic.fromList y__)
instance Data.ProtoLens.Field.HasField Http "vec'rules"
           (Data.Vector.Vector HttpRule)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _Http'rules
               (\ x__ y__ -> x__{_Http'rules = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Message Http where
        messageName _ = Data.Text.pack "google.api.Http"
        fieldsByTag
          = let rules__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "rules"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor HttpRule)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Data.ProtoLens.Field.field @"rules"))
                      :: Data.ProtoLens.FieldDescriptor Http
              in
              Data.Map.fromList [(Data.ProtoLens.Tag 1, rules__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _Http'_unknownFields
              (\ x__ y__ -> x__{_Http'_unknownFields = y__})
        defMessage
          = Http{_Http'rules = Data.Vector.Generic.empty,
                 _Http'_unknownFields = ([])}
        parseMessage
          = let loop ::
                     Http ->
                       Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector
                         Data.ProtoLens.Encoding.Growing.RealWorld
                         HttpRule
                         -> Data.ProtoLens.Encoding.Bytes.Parser Http
                loop x mutable'rules
                  = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
                       if end then
                         do frozen'rules <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                              (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                 mutable'rules)
                            let missing = [] in
                              if Prelude.null missing then Prelude.return () else
                                Prelude.fail
                                  (("Missing required fields: ") Prelude.++
                                     Prelude.show (missing :: ([Prelude.String])))
                            Prelude.return
                              (Lens.Family2.over Data.ProtoLens.unknownFields
                                 (\ !t -> Prelude.reverse t)
                                 (Lens.Family2.set (Data.ProtoLens.Field.field @"vec'rules")
                                    frozen'rules
                                    x))
                         else
                         do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                            case tag of
                                10 -> do !y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                   Data.ProtoLens.Encoding.Bytes.isolate
                                                     (Prelude.fromIntegral len)
                                                     Data.ProtoLens.parseMessage)
                                                 Data.ProtoLens.Encoding.Bytes.<?> "rules"
                                         v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                (Data.ProtoLens.Encoding.Growing.append
                                                   mutable'rules
                                                   y)
                                         loop x v
                                wire -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                                   wire
                                           loop
                                             (Lens.Family2.over Data.ProtoLens.unknownFields
                                                (\ !t -> (:) y t)
                                                x)
                                             mutable'rules
              in
              (do mutable'rules <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                     Data.ProtoLens.Encoding.Growing.new
                  loop Data.ProtoLens.defMessage mutable'rules)
                Data.ProtoLens.Encoding.Bytes.<?> "Http"
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
                  (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'rules") _x))
                 Data.Monoid.<>
                 Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData Http where
        rnf
          = (\ x__ ->
               Control.DeepSeq.deepseq (_Http'_unknownFields x__)
                 (Control.DeepSeq.deepseq (_Http'rules x__) (())))
{- | Fields :

    * 'Proto.Google.Api.Http_Fields.selector' @:: Lens' HttpRule Data.Text.Text@
    * 'Proto.Google.Api.Http_Fields.body' @:: Lens' HttpRule Data.Text.Text@
    * 'Proto.Google.Api.Http_Fields.additionalBindings' @:: Lens' HttpRule [HttpRule]@
    * 'Proto.Google.Api.Http_Fields.vec'additionalBindings' @:: Lens' HttpRule (Data.Vector.Vector HttpRule)@
    * 'Proto.Google.Api.Http_Fields.maybe'pattern'' @:: Lens' HttpRule (Prelude.Maybe HttpRule'Pattern)@
    * 'Proto.Google.Api.Http_Fields.maybe'get' @:: Lens' HttpRule (Prelude.Maybe Data.Text.Text)@
    * 'Proto.Google.Api.Http_Fields.get' @:: Lens' HttpRule Data.Text.Text@
    * 'Proto.Google.Api.Http_Fields.maybe'put' @:: Lens' HttpRule (Prelude.Maybe Data.Text.Text)@
    * 'Proto.Google.Api.Http_Fields.put' @:: Lens' HttpRule Data.Text.Text@
    * 'Proto.Google.Api.Http_Fields.maybe'post' @:: Lens' HttpRule (Prelude.Maybe Data.Text.Text)@
    * 'Proto.Google.Api.Http_Fields.post' @:: Lens' HttpRule Data.Text.Text@
    * 'Proto.Google.Api.Http_Fields.maybe'delete' @:: Lens' HttpRule (Prelude.Maybe Data.Text.Text)@
    * 'Proto.Google.Api.Http_Fields.delete' @:: Lens' HttpRule Data.Text.Text@
    * 'Proto.Google.Api.Http_Fields.maybe'patch' @:: Lens' HttpRule (Prelude.Maybe Data.Text.Text)@
    * 'Proto.Google.Api.Http_Fields.patch' @:: Lens' HttpRule Data.Text.Text@
    * 'Proto.Google.Api.Http_Fields.maybe'custom' @:: Lens' HttpRule (Prelude.Maybe CustomHttpPattern)@
    * 'Proto.Google.Api.Http_Fields.custom' @:: Lens' HttpRule CustomHttpPattern@
 -}
data HttpRule = HttpRule{_HttpRule'selector :: !Data.Text.Text,
                         _HttpRule'body :: !Data.Text.Text,
                         _HttpRule'additionalBindings :: !(Data.Vector.Vector HttpRule),
                         _HttpRule'pattern' :: !(Prelude.Maybe HttpRule'Pattern),
                         _HttpRule'_unknownFields :: !Data.ProtoLens.FieldSet}
                  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show HttpRule where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
data HttpRule'Pattern = HttpRule'Get !Data.Text.Text
                      | HttpRule'Put !Data.Text.Text
                      | HttpRule'Post !Data.Text.Text
                      | HttpRule'Delete !Data.Text.Text
                      | HttpRule'Patch !Data.Text.Text
                      | HttpRule'Custom !CustomHttpPattern
                          deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField HttpRule "selector"
           (Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _HttpRule'selector
               (\ x__ y__ -> x__{_HttpRule'selector = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField HttpRule "body"
           (Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _HttpRule'body
               (\ x__ y__ -> x__{_HttpRule'body = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField HttpRule
           "additionalBindings"
           ([HttpRule])
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _HttpRule'additionalBindings
               (\ x__ y__ -> x__{_HttpRule'additionalBindings = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens Data.Vector.Generic.toList
                (\ _ y__ -> Data.Vector.Generic.fromList y__)
instance Data.ProtoLens.Field.HasField HttpRule
           "vec'additionalBindings"
           (Data.Vector.Vector HttpRule)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _HttpRule'additionalBindings
               (\ x__ y__ -> x__{_HttpRule'additionalBindings = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField HttpRule "maybe'pattern'"
           (Prelude.Maybe HttpRule'Pattern)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _HttpRule'pattern'
               (\ x__ y__ -> x__{_HttpRule'pattern' = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField HttpRule "maybe'get"
           (Prelude.Maybe Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _HttpRule'pattern'
               (\ x__ y__ -> x__{_HttpRule'pattern' = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens
                (\ x__ ->
                   case x__ of
                       Prelude.Just (HttpRule'Get x__val) -> Prelude.Just x__val
                       _otherwise -> Prelude.Nothing)
                (\ _ y__ -> Prelude.fmap HttpRule'Get y__)
instance Data.ProtoLens.Field.HasField HttpRule "get"
           (Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _HttpRule'pattern'
               (\ x__ y__ -> x__{_HttpRule'pattern' = y__}))
              Prelude..
              (Lens.Family2.Unchecked.lens
                 (\ x__ ->
                    case x__ of
                        Prelude.Just (HttpRule'Get x__val) -> Prelude.Just x__val
                        _otherwise -> Prelude.Nothing)
                 (\ _ y__ -> Prelude.fmap HttpRule'Get y__))
                Prelude.. Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault
instance Data.ProtoLens.Field.HasField HttpRule "maybe'put"
           (Prelude.Maybe Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _HttpRule'pattern'
               (\ x__ y__ -> x__{_HttpRule'pattern' = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens
                (\ x__ ->
                   case x__ of
                       Prelude.Just (HttpRule'Put x__val) -> Prelude.Just x__val
                       _otherwise -> Prelude.Nothing)
                (\ _ y__ -> Prelude.fmap HttpRule'Put y__)
instance Data.ProtoLens.Field.HasField HttpRule "put"
           (Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _HttpRule'pattern'
               (\ x__ y__ -> x__{_HttpRule'pattern' = y__}))
              Prelude..
              (Lens.Family2.Unchecked.lens
                 (\ x__ ->
                    case x__ of
                        Prelude.Just (HttpRule'Put x__val) -> Prelude.Just x__val
                        _otherwise -> Prelude.Nothing)
                 (\ _ y__ -> Prelude.fmap HttpRule'Put y__))
                Prelude.. Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault
instance Data.ProtoLens.Field.HasField HttpRule "maybe'post"
           (Prelude.Maybe Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _HttpRule'pattern'
               (\ x__ y__ -> x__{_HttpRule'pattern' = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens
                (\ x__ ->
                   case x__ of
                       Prelude.Just (HttpRule'Post x__val) -> Prelude.Just x__val
                       _otherwise -> Prelude.Nothing)
                (\ _ y__ -> Prelude.fmap HttpRule'Post y__)
instance Data.ProtoLens.Field.HasField HttpRule "post"
           (Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _HttpRule'pattern'
               (\ x__ y__ -> x__{_HttpRule'pattern' = y__}))
              Prelude..
              (Lens.Family2.Unchecked.lens
                 (\ x__ ->
                    case x__ of
                        Prelude.Just (HttpRule'Post x__val) -> Prelude.Just x__val
                        _otherwise -> Prelude.Nothing)
                 (\ _ y__ -> Prelude.fmap HttpRule'Post y__))
                Prelude.. Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault
instance Data.ProtoLens.Field.HasField HttpRule "maybe'delete"
           (Prelude.Maybe Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _HttpRule'pattern'
               (\ x__ y__ -> x__{_HttpRule'pattern' = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens
                (\ x__ ->
                   case x__ of
                       Prelude.Just (HttpRule'Delete x__val) -> Prelude.Just x__val
                       _otherwise -> Prelude.Nothing)
                (\ _ y__ -> Prelude.fmap HttpRule'Delete y__)
instance Data.ProtoLens.Field.HasField HttpRule "delete"
           (Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _HttpRule'pattern'
               (\ x__ y__ -> x__{_HttpRule'pattern' = y__}))
              Prelude..
              (Lens.Family2.Unchecked.lens
                 (\ x__ ->
                    case x__ of
                        Prelude.Just (HttpRule'Delete x__val) -> Prelude.Just x__val
                        _otherwise -> Prelude.Nothing)
                 (\ _ y__ -> Prelude.fmap HttpRule'Delete y__))
                Prelude.. Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault
instance Data.ProtoLens.Field.HasField HttpRule "maybe'patch"
           (Prelude.Maybe Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _HttpRule'pattern'
               (\ x__ y__ -> x__{_HttpRule'pattern' = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens
                (\ x__ ->
                   case x__ of
                       Prelude.Just (HttpRule'Patch x__val) -> Prelude.Just x__val
                       _otherwise -> Prelude.Nothing)
                (\ _ y__ -> Prelude.fmap HttpRule'Patch y__)
instance Data.ProtoLens.Field.HasField HttpRule "patch"
           (Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _HttpRule'pattern'
               (\ x__ y__ -> x__{_HttpRule'pattern' = y__}))
              Prelude..
              (Lens.Family2.Unchecked.lens
                 (\ x__ ->
                    case x__ of
                        Prelude.Just (HttpRule'Patch x__val) -> Prelude.Just x__val
                        _otherwise -> Prelude.Nothing)
                 (\ _ y__ -> Prelude.fmap HttpRule'Patch y__))
                Prelude.. Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault
instance Data.ProtoLens.Field.HasField HttpRule "maybe'custom"
           (Prelude.Maybe CustomHttpPattern)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _HttpRule'pattern'
               (\ x__ y__ -> x__{_HttpRule'pattern' = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens
                (\ x__ ->
                   case x__ of
                       Prelude.Just (HttpRule'Custom x__val) -> Prelude.Just x__val
                       _otherwise -> Prelude.Nothing)
                (\ _ y__ -> Prelude.fmap HttpRule'Custom y__)
instance Data.ProtoLens.Field.HasField HttpRule "custom"
           (CustomHttpPattern)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _HttpRule'pattern'
               (\ x__ y__ -> x__{_HttpRule'pattern' = y__}))
              Prelude..
              (Lens.Family2.Unchecked.lens
                 (\ x__ ->
                    case x__ of
                        Prelude.Just (HttpRule'Custom x__val) -> Prelude.Just x__val
                        _otherwise -> Prelude.Nothing)
                 (\ _ y__ -> Prelude.fmap HttpRule'Custom y__))
                Prelude.. Data.ProtoLens.maybeLens Data.ProtoLens.defMessage
instance Data.ProtoLens.Message HttpRule where
        messageName _ = Data.Text.pack "google.api.HttpRule"
        fieldsByTag
          = let selector__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "selector"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"selector"))
                      :: Data.ProtoLens.FieldDescriptor HttpRule
                body__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "body"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"body"))
                      :: Data.ProtoLens.FieldDescriptor HttpRule
                additionalBindings__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "additional_bindings"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor HttpRule)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Data.ProtoLens.Field.field @"additionalBindings"))
                      :: Data.ProtoLens.FieldDescriptor HttpRule
                get__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "get"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.OptionalField
                         (Data.ProtoLens.Field.field @"maybe'get"))
                      :: Data.ProtoLens.FieldDescriptor HttpRule
                put__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "put"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.OptionalField
                         (Data.ProtoLens.Field.field @"maybe'put"))
                      :: Data.ProtoLens.FieldDescriptor HttpRule
                post__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "post"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.OptionalField
                         (Data.ProtoLens.Field.field @"maybe'post"))
                      :: Data.ProtoLens.FieldDescriptor HttpRule
                delete__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "delete"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.OptionalField
                         (Data.ProtoLens.Field.field @"maybe'delete"))
                      :: Data.ProtoLens.FieldDescriptor HttpRule
                patch__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "patch"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.OptionalField
                         (Data.ProtoLens.Field.field @"maybe'patch"))
                      :: Data.ProtoLens.FieldDescriptor HttpRule
                custom__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "custom"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor CustomHttpPattern)
                      (Data.ProtoLens.OptionalField
                         (Data.ProtoLens.Field.field @"maybe'custom"))
                      :: Data.ProtoLens.FieldDescriptor HttpRule
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, selector__field_descriptor),
                 (Data.ProtoLens.Tag 7, body__field_descriptor),
                 (Data.ProtoLens.Tag 11, additionalBindings__field_descriptor),
                 (Data.ProtoLens.Tag 2, get__field_descriptor),
                 (Data.ProtoLens.Tag 3, put__field_descriptor),
                 (Data.ProtoLens.Tag 4, post__field_descriptor),
                 (Data.ProtoLens.Tag 5, delete__field_descriptor),
                 (Data.ProtoLens.Tag 6, patch__field_descriptor),
                 (Data.ProtoLens.Tag 8, custom__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _HttpRule'_unknownFields
              (\ x__ y__ -> x__{_HttpRule'_unknownFields = y__})
        defMessage
          = HttpRule{_HttpRule'selector = Data.ProtoLens.fieldDefault,
                     _HttpRule'body = Data.ProtoLens.fieldDefault,
                     _HttpRule'additionalBindings = Data.Vector.Generic.empty,
                     _HttpRule'pattern' = Prelude.Nothing,
                     _HttpRule'_unknownFields = ([])}
        parseMessage
          = let loop ::
                     HttpRule ->
                       Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector
                         Data.ProtoLens.Encoding.Growing.RealWorld
                         HttpRule
                         -> Data.ProtoLens.Encoding.Bytes.Parser HttpRule
                loop x mutable'additionalBindings
                  = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
                       if end then
                         do frozen'additionalBindings <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                           (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                              mutable'additionalBindings)
                            let missing = [] in
                              if Prelude.null missing then Prelude.return () else
                                Prelude.fail
                                  (("Missing required fields: ") Prelude.++
                                     Prelude.show (missing :: ([Prelude.String])))
                            Prelude.return
                              (Lens.Family2.over Data.ProtoLens.unknownFields
                                 (\ !t -> Prelude.reverse t)
                                 (Lens.Family2.set
                                    (Data.ProtoLens.Field.field @"vec'additionalBindings")
                                    frozen'additionalBindings
                                    x))
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
                                                Data.ProtoLens.Encoding.Bytes.<?> "selector"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"selector")
                                              y
                                              x)
                                           mutable'additionalBindings
                                58 -> do y <- (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                              Data.ProtoLens.Encoding.Bytes.getBytes
                                                                (Prelude.fromIntegral len)
                                                  Data.ProtoLens.Encoding.Bytes.runEither
                                                    (case Data.Text.Encoding.decodeUtf8' value of
                                                         Prelude.Left err -> Prelude.Left
                                                                               (Prelude.show err)
                                                         Prelude.Right r -> Prelude.Right r))
                                                Data.ProtoLens.Encoding.Bytes.<?> "body"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"body") y
                                              x)
                                           mutable'additionalBindings
                                90 -> do !y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                   Data.ProtoLens.Encoding.Bytes.isolate
                                                     (Prelude.fromIntegral len)
                                                     Data.ProtoLens.parseMessage)
                                                 Data.ProtoLens.Encoding.Bytes.<?>
                                                 "additional_bindings"
                                         v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                (Data.ProtoLens.Encoding.Growing.append
                                                   mutable'additionalBindings
                                                   y)
                                         loop x v
                                18 -> do y <- (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                              Data.ProtoLens.Encoding.Bytes.getBytes
                                                                (Prelude.fromIntegral len)
                                                  Data.ProtoLens.Encoding.Bytes.runEither
                                                    (case Data.Text.Encoding.decodeUtf8' value of
                                                         Prelude.Left err -> Prelude.Left
                                                                               (Prelude.show err)
                                                         Prelude.Right r -> Prelude.Right r))
                                                Data.ProtoLens.Encoding.Bytes.<?> "get"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"get") y
                                              x)
                                           mutable'additionalBindings
                                26 -> do y <- (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                              Data.ProtoLens.Encoding.Bytes.getBytes
                                                                (Prelude.fromIntegral len)
                                                  Data.ProtoLens.Encoding.Bytes.runEither
                                                    (case Data.Text.Encoding.decodeUtf8' value of
                                                         Prelude.Left err -> Prelude.Left
                                                                               (Prelude.show err)
                                                         Prelude.Right r -> Prelude.Right r))
                                                Data.ProtoLens.Encoding.Bytes.<?> "put"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"put") y
                                              x)
                                           mutable'additionalBindings
                                34 -> do y <- (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                              Data.ProtoLens.Encoding.Bytes.getBytes
                                                                (Prelude.fromIntegral len)
                                                  Data.ProtoLens.Encoding.Bytes.runEither
                                                    (case Data.Text.Encoding.decodeUtf8' value of
                                                         Prelude.Left err -> Prelude.Left
                                                                               (Prelude.show err)
                                                         Prelude.Right r -> Prelude.Right r))
                                                Data.ProtoLens.Encoding.Bytes.<?> "post"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"post") y
                                              x)
                                           mutable'additionalBindings
                                42 -> do y <- (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                              Data.ProtoLens.Encoding.Bytes.getBytes
                                                                (Prelude.fromIntegral len)
                                                  Data.ProtoLens.Encoding.Bytes.runEither
                                                    (case Data.Text.Encoding.decodeUtf8' value of
                                                         Prelude.Left err -> Prelude.Left
                                                                               (Prelude.show err)
                                                         Prelude.Right r -> Prelude.Right r))
                                                Data.ProtoLens.Encoding.Bytes.<?> "delete"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"delete")
                                              y
                                              x)
                                           mutable'additionalBindings
                                50 -> do y <- (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                              Data.ProtoLens.Encoding.Bytes.getBytes
                                                                (Prelude.fromIntegral len)
                                                  Data.ProtoLens.Encoding.Bytes.runEither
                                                    (case Data.Text.Encoding.decodeUtf8' value of
                                                         Prelude.Left err -> Prelude.Left
                                                                               (Prelude.show err)
                                                         Prelude.Right r -> Prelude.Right r))
                                                Data.ProtoLens.Encoding.Bytes.<?> "patch"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"patch") y
                                              x)
                                           mutable'additionalBindings
                                66 -> do y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                  Data.ProtoLens.Encoding.Bytes.isolate
                                                    (Prelude.fromIntegral len)
                                                    Data.ProtoLens.parseMessage)
                                                Data.ProtoLens.Encoding.Bytes.<?> "custom"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"custom")
                                              y
                                              x)
                                           mutable'additionalBindings
                                wire -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                                   wire
                                           loop
                                             (Lens.Family2.over Data.ProtoLens.unknownFields
                                                (\ !t -> (:) y t)
                                                x)
                                             mutable'additionalBindings
              in
              (do mutable'additionalBindings <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                  Data.ProtoLens.Encoding.Growing.new
                  loop Data.ProtoLens.defMessage mutable'additionalBindings)
                Data.ProtoLens.Encoding.Bytes.<?> "HttpRule"
        buildMessage
          = (\ _x ->
               (let _v
                      = Lens.Family2.view (Data.ProtoLens.Field.field @"selector") _x
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
                 (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"body") _x
                    in
                    if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                      Data.Monoid.mempty else
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 58) Data.Monoid.<>
                        (((\ bs ->
                             (Data.ProtoLens.Encoding.Bytes.putVarInt
                                (Prelude.fromIntegral (Data.ByteString.length bs)))
                               Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                           Prelude.. Data.Text.Encoding.encodeUtf8)
                          _v)
                   Data.Monoid.<>
                   (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                      (\ _v ->
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 90) Data.Monoid.<>
                           (((\ bs ->
                                (Data.ProtoLens.Encoding.Bytes.putVarInt
                                   (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                              Prelude.. Data.ProtoLens.encodeMessage)
                             _v)
                      (Lens.Family2.view
                         (Data.ProtoLens.Field.field @"vec'additionalBindings")
                         _x))
                     Data.Monoid.<>
                     (case
                        Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'pattern'") _x
                        of
                          (Prelude.Nothing) -> Data.Monoid.mempty
                          Prelude.Just
                            (HttpRule'Get v) -> (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                                                  Data.Monoid.<>
                                                  (((\ bs ->
                                                       (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                          (Prelude.fromIntegral
                                                             (Data.ByteString.length bs)))
                                                         Data.Monoid.<>
                                                         Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                                     Prelude.. Data.Text.Encoding.encodeUtf8)
                                                    v
                          Prelude.Just
                            (HttpRule'Put v) -> (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                                                  Data.Monoid.<>
                                                  (((\ bs ->
                                                       (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                          (Prelude.fromIntegral
                                                             (Data.ByteString.length bs)))
                                                         Data.Monoid.<>
                                                         Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                                     Prelude.. Data.Text.Encoding.encodeUtf8)
                                                    v
                          Prelude.Just
                            (HttpRule'Post v) -> (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                                                   Data.Monoid.<>
                                                   (((\ bs ->
                                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                           (Prelude.fromIntegral
                                                              (Data.ByteString.length bs)))
                                                          Data.Monoid.<>
                                                          Data.ProtoLens.Encoding.Bytes.putBytes
                                                            bs))
                                                      Prelude.. Data.Text.Encoding.encodeUtf8)
                                                     v
                          Prelude.Just
                            (HttpRule'Delete v) -> (Data.ProtoLens.Encoding.Bytes.putVarInt 42)
                                                     Data.Monoid.<>
                                                     (((\ bs ->
                                                          (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                             (Prelude.fromIntegral
                                                                (Data.ByteString.length bs)))
                                                            Data.Monoid.<>
                                                            Data.ProtoLens.Encoding.Bytes.putBytes
                                                              bs))
                                                        Prelude.. Data.Text.Encoding.encodeUtf8)
                                                       v
                          Prelude.Just
                            (HttpRule'Patch v) -> (Data.ProtoLens.Encoding.Bytes.putVarInt 50)
                                                    Data.Monoid.<>
                                                    (((\ bs ->
                                                         (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                            (Prelude.fromIntegral
                                                               (Data.ByteString.length bs)))
                                                           Data.Monoid.<>
                                                           Data.ProtoLens.Encoding.Bytes.putBytes
                                                             bs))
                                                       Prelude.. Data.Text.Encoding.encodeUtf8)
                                                      v
                          Prelude.Just
                            (HttpRule'Custom v) -> (Data.ProtoLens.Encoding.Bytes.putVarInt 66)
                                                     Data.Monoid.<>
                                                     (((\ bs ->
                                                          (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                             (Prelude.fromIntegral
                                                                (Data.ByteString.length bs)))
                                                            Data.Monoid.<>
                                                            Data.ProtoLens.Encoding.Bytes.putBytes
                                                              bs))
                                                        Prelude.. Data.ProtoLens.encodeMessage)
                                                       v)
                       Data.Monoid.<>
                       Data.ProtoLens.Encoding.Wire.buildFieldSet
                         (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData HttpRule where
        rnf
          = (\ x__ ->
               Control.DeepSeq.deepseq (_HttpRule'_unknownFields x__)
                 (Control.DeepSeq.deepseq (_HttpRule'selector x__)
                    (Control.DeepSeq.deepseq (_HttpRule'body x__)
                       (Control.DeepSeq.deepseq (_HttpRule'additionalBindings x__)
                          (Control.DeepSeq.deepseq (_HttpRule'pattern' x__) (()))))))
instance Control.DeepSeq.NFData HttpRule'Pattern where
        rnf (HttpRule'Get x__) = Control.DeepSeq.rnf x__
        rnf (HttpRule'Put x__) = Control.DeepSeq.rnf x__
        rnf (HttpRule'Post x__) = Control.DeepSeq.rnf x__
        rnf (HttpRule'Delete x__) = Control.DeepSeq.rnf x__
        rnf (HttpRule'Patch x__) = Control.DeepSeq.rnf x__
        rnf (HttpRule'Custom x__) = Control.DeepSeq.rnf x__
_HttpRule'Get ::
              Data.ProtoLens.Prism.Prism' HttpRule'Pattern Data.Text.Text
_HttpRule'Get
  = Data.ProtoLens.Prism.prism' HttpRule'Get
      (\ p__ ->
         case p__ of
             HttpRule'Get p__val -> Prelude.Just p__val
             _otherwise -> Prelude.Nothing)
_HttpRule'Put ::
              Data.ProtoLens.Prism.Prism' HttpRule'Pattern Data.Text.Text
_HttpRule'Put
  = Data.ProtoLens.Prism.prism' HttpRule'Put
      (\ p__ ->
         case p__ of
             HttpRule'Put p__val -> Prelude.Just p__val
             _otherwise -> Prelude.Nothing)
_HttpRule'Post ::
               Data.ProtoLens.Prism.Prism' HttpRule'Pattern Data.Text.Text
_HttpRule'Post
  = Data.ProtoLens.Prism.prism' HttpRule'Post
      (\ p__ ->
         case p__ of
             HttpRule'Post p__val -> Prelude.Just p__val
             _otherwise -> Prelude.Nothing)
_HttpRule'Delete ::
                 Data.ProtoLens.Prism.Prism' HttpRule'Pattern Data.Text.Text
_HttpRule'Delete
  = Data.ProtoLens.Prism.prism' HttpRule'Delete
      (\ p__ ->
         case p__ of
             HttpRule'Delete p__val -> Prelude.Just p__val
             _otherwise -> Prelude.Nothing)
_HttpRule'Patch ::
                Data.ProtoLens.Prism.Prism' HttpRule'Pattern Data.Text.Text
_HttpRule'Patch
  = Data.ProtoLens.Prism.prism' HttpRule'Patch
      (\ p__ ->
         case p__ of
             HttpRule'Patch p__val -> Prelude.Just p__val
             _otherwise -> Prelude.Nothing)
_HttpRule'Custom ::
                 Data.ProtoLens.Prism.Prism' HttpRule'Pattern CustomHttpPattern
_HttpRule'Custom
  = Data.ProtoLens.Prism.prism' HttpRule'Custom
      (\ p__ ->
         case p__ of
             HttpRule'Custom p__val -> Prelude.Just p__val
             _otherwise -> Prelude.Nothing)