{-# LANGUAGE OverloadedStrings #-}

module OpenTelemetry.Network.Wai.Middleware where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Types
import Network.Wai
import OpenTelemetry.Implicit
import OpenTelemetry.Propagation

-- Semantic conventions for HTTP spans:
-- https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/data-http.md

middleware :: Application -> Application
middleware app = \req sendResp -> do
  withSpan "WAI handler" $ do
    case propagateFromHeaders w3cTraceContext (requestHeaders req) of
      Just ctx -> setParentSpanContext ctx
      _ -> pure ()
    setTag "span.kind" ("server" :: T.Text)
    setTag "component" ("http" :: T.Text)
    setTag "http.method" $ T.decodeUtf8 (requestMethod req)
    setTag "http.target" $ T.decodeUtf8 (rawPathInfo req)
    setTag "http.flavor" $ show (httpVersion req)
    app req $ \resp -> do
      setTag "http.status_code" (statusCode $ responseStatus resp)
      sendResp resp
