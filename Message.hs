{-# LANGUAGE TemplateHaskell, OverloadedStrings, NamedFieldPuns #-}

module Message where

import JsonInstances
import JsonUtils

import Data.Aeson
import Data.Aeson.TH
import Data.Int (Int64)
import Data.Text
import Language.Haskell.TH

import Language.Haskell.TH

data ClientMessage
    = ClientFCall  { cFunName :: Name
                                 , cFunId :: Int64
                                 , cFunArgs :: [Value] }
    | LOLOLOLOLOL
                                 deriving (Show)

data ServerMessage
    = ServerFunctionReturn { sFunId :: Int64
                                          , sRetVal :: Value }
    | LOLOLOLOLOAL
                                          deriving (Show)

-- TODO TH
instance ToJSON ServerMessage where
    toJSON (ServerFunctionReturn { sFunId, sRetVal })
        = constr "ServerFunctionReturn" $ object [ "sFunId" .= toJSON sFunId
                                                 , "sRetVal" .= toJSON sRetVal
                                                 ]
$(deriveToJSON id ''ClientMessage)
$(deriveFromJSON id ''ClientMessage)
