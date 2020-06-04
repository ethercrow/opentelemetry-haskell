{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Resource where

import Attribute
import Data.Aeson.TH
import Data.List as L
import Data.Maybe
import Data.ProtoLens (defMessage)
import Data.Text

import Json

import Lens.Micro

import Proto.Opentelemetry.Proto.Common.V1.Common as C
import Proto.Opentelemetry.Proto.Resource.V1.Resource as R
import Proto.Opentelemetry.Proto.Resource.V1.Resource_Fields as R

-- header structures to list of KeyValueAttribute
class ConversionTo src dst where
    convertTo :: src -> dst

data ServiceHeader = ServiceHeader
    { _sName       :: Text
    , _sNameSpace  :: Maybe Text
    , _sInstanceId :: Text
    , _sVersion    :: Maybe Text
    } deriving (Eq, Show)

$(deriveJSON (jsonOpts 2) ''ServiceHeader)

instance ConversionTo ServiceHeader [C.AttributeKeyValue] where
    convertTo ServiceHeader{..} = mandatory ++ extras
        where
          sa k = strAttr (("service."::Text) <> k)
          mandatory = [ sa "name" _sName, sa "instance.id" _sInstanceId ]
          extras = catMaybes [
                    fmap (sa "namespace") _sNameSpace,
                    fmap (sa "version") _sVersion
                   ]

data ExtraResourceHeader
    = TelemetrySdk
      { _tlsSdkName :: Text
      , _tlsSdkLanguage :: Text
      }
    | ComputeUnitContainer
      { _cucName :: Text
      , _cucImageName :: Text
      }
    | ComputeInstanceHost
      { _cihHost :: Text
      , _cihId   :: Text
      }
    deriving (Eq, Show)

$(deriveJSON (jsonOpts 4) ''ExtraResourceHeader)

instance ConversionTo ExtraResourceHeader [C.AttributeKeyValue] where
    convertTo TelemetrySdk{..} =
        [ strAttr "telemetry.sdk.name" _tlsSdkName
        , strAttr "telemetry.sdk.language" "haskell"
        ]
    convertTo ComputeUnitContainer{..} =
        [ strAttr "container.name" _cucName
        , strAttr "container.image.name" _cucImageName
        ]
    convertTo ComputeInstanceHost{..} =
        [ strAttr "host.name" _cihHost
        , strAttr "host.hostname" _cihHost
        , strAttr "host.id" _cihId
        ]


data ResourceHeader = ResourceHeader
    { _rServiceHeader :: ServiceHeader
    , _rExtraHeaders :: [ ExtraResourceHeader ]
    } deriving (Eq, Show)

$(deriveJSON (jsonOpts 2) ''ResourceHeader)

instance ConversionTo ResourceHeader R.Resource where
    convertTo ResourceHeader{..} =
        defMessage
          & R.attributes  .~ attrs
          & R.droppedAttributesCount .~ 0
              where
                c a = (convertTo a) :: [C.AttributeKeyValue]
                attrs = c _rServiceHeader ++ (L.concat $ fmap c _rExtraHeaders)
