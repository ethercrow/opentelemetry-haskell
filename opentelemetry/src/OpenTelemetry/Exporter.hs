module OpenTelemetry.Exporter where

data ExportResult
  = ExportSuccess
  | ExportFailedRetryable
  | ExportFailedNotRetryable
  deriving (Show, Eq)

data Exporter thing
  = Exporter
      { export :: [thing] -> IO ExportResult,
        shutdown :: IO ()
      }

noopExporter :: Exporter whatever
noopExporter = Exporter (const (pure ExportFailedNotRetryable)) (pure ())
