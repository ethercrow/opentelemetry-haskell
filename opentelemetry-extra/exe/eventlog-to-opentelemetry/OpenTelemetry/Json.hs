module OpenTelemetry.Json (jsonOpts) where

import Data.Aeson
import Data.Char

-- extra file due TH constraint
jsonOpts :: Int -> Options
jsonOpts toDrop = defaultOptions
                 { fieldLabelModifier = drop toDrop
                 , omitNothingFields = True
                 , constructorTagModifier = map toLower
                 }
