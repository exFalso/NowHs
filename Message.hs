{-# LANGUAGE TemplateHaskell #-}

module Message where

import JsonInstances
import FunctionCall

import Data.Aeson
import Data.Aeson.TH

import Language.Haskell.TH

data ClientMessage
    = ClientFCall FunctionCall
$(deriveFromJSON id ''ClientMessage)

data ServerMessage
    = ServerFRet { funcId  :: Integer
                 , funcRet :: Value }
$(deriveToJSON id ''ClientMessage)
