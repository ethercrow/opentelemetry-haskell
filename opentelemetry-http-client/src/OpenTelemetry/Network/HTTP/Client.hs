{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module OpenTelemetry.Network.HTTP.Client where

import Data.String
import qualified Data.Text.Encoding as T
import Network.HTTP.Client
import Network.HTTP.Types
import OpenTelemetry.Common
import OpenTelemetry.Implicit
import OpenTelemetry.Propagation

middleware :: ManagerSettings -> ManagerSettings
middleware m =
  m
    { managerModifyRequest = \req -> do
        setTag @String "span.kind" "client"
        case T.decodeUtf8' (host req) of
          Right hostname -> setTag "http.host" hostname
          Left _ -> pure ()
        case T.decodeUtf8' (path req) of
          Right p -> setTag "http.url" p
          Left _ -> pure ()
        msp <- getCurrentActiveSpan
        let req' = case msp of
              Nothing -> req
              Just (spanContext -> ctx) ->
                let propagationHeaders =
                      [ (fromString k, v)
                        | (k, v) <- inject W3CTraceContext ctx
                      ]
                 in req {requestHeaders = requestHeaders req <> propagationHeaders}
        result <- managerModifyRequest m req'
        pure result,
      managerModifyResponse = \resp -> do
        setTag "http.status" $ statusCode (responseStatus resp)
        managerModifyResponse m resp
    }
