{-# LANGUAGE OverloadedStrings #-}

module OpenTelemetry.Network.Wai.Middleware where

import qualified Data.ByteString.Char8 as BS8
import Network.HTTP.Types
import Network.Wai
import OpenTelemetry.Eventlog
import OpenTelemetry.Propagation

-- Semantic conventions for HTTP spans:
-- https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/trace/semantic_conventions/http.md

middleware :: Application -> Application
middleware app = \req sendResp -> do
  withSpan "WAI handler" $ do
    case propagateFromHeaders w3cTraceContext (requestHeaders req) of
      Just ctx -> setParentSpanContext ctx
      _ -> pure ()
    setTag "span.kind" "server"
    setTag "component" "http"
    setTag "http.method" $ requestMethod req
    setTag "http.target" $ rawPathInfo req
    setTag "http.flavor" $ BS8.pack $ show (httpVersion req)
    app req $ \resp -> do
      setTag "http.status_code" (BS8.pack $ show $ statusCode $ responseStatus resp)
      sendResp resp
