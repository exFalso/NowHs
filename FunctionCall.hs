{-# LANGUAGE TemplateHaskell #-}

module FunctionCall where

import JsonInstances

import Data.Aeson
import Data.Aeson.TH
import Language.Haskell.TH

data FunctionCall = FunctionCall { funName :: Name
                                 , funArgs :: [Value] }
                  deriving (Show)
$(deriveJSON id ''FunctionCall)
