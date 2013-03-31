{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, ExistentialQuantification, OverloadedStrings #-}
module Schema where

import JsonInstances
import JsonUtils

import Data.Aeson
import Data.Aeson.TH
import Data.Text

import Language.Haskell.TH
import Language.Haskell.TH.Lift

import Control.Applicative

data PrimType
    = Num
    | Str
    | Boolean
    | Obj
    deriving (Show, Eq, Ord)
$(deriveLift ''PrimType)

instance ToJSON PrimType where
    toJSON Num     = String "number"
    toJSON Str     = String "string"
    toJSON Obj     = String "object"
    toJSON Boolean = String "boolean"

-- a type that can be qualified with required etc
-- also used for interface function arguments
data SchemaType
    = Prim PrimType
    | SchemaName Name           -- name of haskell type, js will make this a reference to the schema object
    deriving (Show, Eq, Ord)
$(deriveLift ''SchemaType)

instance ToJSON SchemaType where
    toJSON (Prim ty)         = constr "Prim" ty
    toJSON (SchemaName name) = constr "SchemaName" name

data SchemaQualifier
    = Optional
    | Repeated
    | Required
    deriving (Show, Eq, Ord)
$(deriveLift ''SchemaQualifier)

instance ToJSON SchemaQualifier where
    toJSON = toJSON . show

data SchemaField
    = SchemaField SchemaQualifier SchemaType
    deriving (Show, Eq, Ord)
$(deriveLift ''SchemaField)
$(deriveToJSON id ''SchemaField)

-- indexed by the corresponding haskell type
data Schema a
    = Schema { schemaName :: Name -- the corresponding haskell type's name
             , schemaFields :: [(String, SchemaField)] }
    deriving (Show, Eq, Ord)
$(deriveToJSON id ''Schema)

-- manual instance as deriveLift doesn't handle phantom types well
instance Lift (Schema a) where
    lift (Schema n fs) = [| Schema $(lift n) $(lift fs) |]

class HasSchema a where
    schema :: Schema a

-- for the external interface
-- TODO remove (ToJSON a) restriction after no-context-on-phantom branch has been merged
data AnySchema = forall a. (ToJSON a) => MkAnySchema { unMkAnySchema :: (Schema a) }
instance ToJSON AnySchema where
    toJSON (MkAnySchema s) = toJSON s

instance Lift AnySchema where
    lift (MkAnySchema sch) = [| MkAnySchema $(lift sch) |]

instance Show AnySchema where
    show (MkAnySchema sch) = show sch

instance Eq AnySchema where
    (MkAnySchema (Schema n1 _)) == (MkAnySchema (Schema n2 _)) = n1 == n2

instance Ord AnySchema where
    (MkAnySchema (Schema n1 _)) <= (MkAnySchema (Schema n2 _)) = n1 <= n2


-- instance FromJSON AnySchema where
--     parseJSON v = MkAnySchema <$> parseJSON v

-- instance FromJSON (forall a. Schema a) where
--     parseJSON v = MkAnySchema <$> parseJSON v

{-

function1 :: (MonadNowHs m) => Int -> m ()

interfaceF [ 'function1
           , 'function2
           ]
interfaceM [ [| module1 |] ]


download interface:
  ser:[CT] create the schemas of datatypes using the functions' typedef
  ser: get schemas
  js: download schemas and function signatures, generate js interface functions
     the functions should serialise function calls after checking schema correctness of arguments

function call (from js):
  ser: [CT] create parsers for custom datatypes (schema not enough*) (DONE by Aeson!!)
  ser: deserialise arguments, call relevant function

*haskell types -> schemadef NOT INJECTIVE
-}


