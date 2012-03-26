{-# LANGUAGE TemplateHaskell #-}

module Message where

import FunctionCall
import Data.Aeson.TH

data ClientMessage
    = ClientFCall FunctionCall
$(deriveJSON id ClientMessage)
