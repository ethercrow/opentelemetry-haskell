module OpenTelemetry.Implicit where

import Control.Monad.Catch
import qualified Data.Text as T

withSpan :: MonadCatch m => T.Text -> m a -> m a
withSpan operation action = error "withSpan: not implemented"

setTag :: forall value m. Monad m => T.Text -> value -> m ()
setTag k v = error "setTag: not implemented"

addLog :: forall value m. Monad m => T.Text -> value -> m ()
addLog k v = error "addLog: not implemented"
