{-# LANGUAGE FlexibleContexts #-}
module Attribute where

import Data.ProtoLens (defMessage)
import Data.Text
import Lens.Micro
import OpenTelemetry.Common
import qualified Proto.Opentelemetry.Proto.Common.V1.Common as C
import qualified Proto.Opentelemetry.Proto.Common.V1.Common_Fields as C


strAttr :: Text -> Text -> C.AttributeKeyValue
strAttr attrName attrVal = defMessage
                   & C.key .~ attrName
                   & C.type' .~ C.AttributeKeyValue'STRING
                   & C.stringValue .~ attrVal

tagToAttribute :: Text -> TagValue -> C.AttributeKeyValue
tagToAttribute name val = defMessage & C.key .~ name & (setVal val)
    where
      setVal (StringTagValue (TagVal v)) =
          (C.type' .~ C.AttributeKeyValue'STRING) . (C.stringValue .~ v)
      setVal (BoolTagValue v) =
          (C.type' .~ C.AttributeKeyValue'BOOL) . (C.boolValue .~ v)
      setVal (IntTagValue v) =
          (C.type' .~ C.AttributeKeyValue'INT) . (C.intValue .~ fromIntegral v)
      setVal (DoubleTagValue v) =
          (C.type' .~ C.AttributeKeyValue'DOUBLE) . (C.doubleValue .~ v)
