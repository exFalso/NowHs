{-# LANGUAGE TemplateHaskell #-}
module Function where

import Schema
import JsonInstances

import Language.Haskell.TH
import Language.Haskell.TH.Lift
import Data.Aeson.TH

data Function = Function { functionName     :: Name
                         , functionArgTypes :: [SchemaField]
                         , functionRetType  :: SchemaField }
              deriving (Show)
$(deriveToJSON id ''Function)
$(deriveLift ''Function)