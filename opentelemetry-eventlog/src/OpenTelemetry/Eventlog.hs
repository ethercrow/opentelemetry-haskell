module OpenTelemetry.Eventlog
  ( beginSpan,
    endSpan,
    setTag,
    addEvent,
    withSpan,
  )
where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Debug.Trace
import Text.Printf

beginSpan :: String -> IO ()
beginSpan operation = traceEventIO (printf "ot1 begin span %s" operation)

endSpan :: IO ()
endSpan = traceEventIO (printf "ot1 end span")

setTag :: String -> String -> IO ()
setTag k v = traceEventIO (printf "ot1 set tag %s %s" k v)

addEvent :: String -> String -> IO ()
addEvent k v = traceEventIO (printf "ot1 add event %s %s" k v)

withSpan :: forall m a. (MonadIO m, MonadMask m) => String -> m a -> m a
withSpan operation action =
  fst
    <$> generalBracket
      (liftIO $ beginSpan operation)
      (\_span _exitcase -> liftIO endSpan)
      (\_span -> action)
