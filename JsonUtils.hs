{-# LANGUAGE OverloadedStrings #-}

module JsonUtils (
    constr
) where

import Data.Aeson
import Data.Text


constr :: (ToJSON a) => Text -> a -> Value
constr ty val = object [ "type" .= ty
                       , "value" .= val ]
