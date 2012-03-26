module JsonInstances where

import Data.Aeson
import Language.Haskell.TH

import Control.Applicative

instance FromJSON Name where
    parseJSON v = mkName <$> parseJSON v

instance ToJSON Name where
    toJSON = toJSON . show