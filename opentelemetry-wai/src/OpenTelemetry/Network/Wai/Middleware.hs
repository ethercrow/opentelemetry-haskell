{-# LANGUAGE OverloadedStrings #-}

module OpenTelemetry.Network.Wai.Middleware where

import qualified Data.ByteString.Char8 as BS8
import Network.HTTP.Types
import Network.Wai
import OpenTelemetry.Eventlog
import OpenTelemetry.Propagation

-- Semantic conventions for HTTP spans:
-- https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/trace/semantic_conventions/http.md

requestCounter :: Counter
requestCounter = Counter "requests"

middleware :: Application -> Application
middleware app = \req sendResp -> do
  withSpan "WAI handler" $ \sp -> do
    add requestCounter 1
    case propagateFromHeaders w3cTraceContext (requestHeaders req) of
      Just ctx -> setParentSpanContext sp ctx
      _ -> pure ()
    setTag sp "span.kind" "server"
    setTag sp "component" "http"
    setTag sp "http.method" $ requestMethod req
    setTag sp "http.target" $ rawPathInfo req
    setTag sp "http.flavor" $ BS8.pack $ show (httpVersion req)
    app req $ \resp -> do
      setTag sp "http.status_code" (BS8.pack $ show $ statusCode $ responseStatus resp)
      sendResp resp
