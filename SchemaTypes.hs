{-# LANGUAGE TemplateHaskell #-}
module SchemaTypes where

import Schema

import Data.Aeson

-- collections
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.HashSet as HashSet
import qualified Data.IntSet as IntSet
import qualified Data.Vector as Vector

-- primitives
import Data.Word
import Data.Int
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import Language.Haskell.TH.Syntax

-- for primitives that map to primitive js types
mapNamePrimTyp :: Map.Map Name PrimType
mapNamePrimTyp
    = Map.fromList [ (''String,        Str)
                   , (''Char,          Str)
                   , (''T.Text,        Str)
                   , (''LT.Text,       Str)
                   , (''B.ByteString,  Str)
                   , (''LB.ByteString, Str)
                     
                   , (''Int,     Num)
                   , (''Integer, Num)
                   , (''Word64,  Num)
                   , (''Word32,  Num)
                   , (''Word16,  Num)
                   , (''Word8,   Num)
                   , (''Word,    Num)
                   , (''Int64,   Num)
                   , (''Int32,   Num)
                   , (''Int16,   Num)
                   , (''Int8,    Num)
                     
                   , (''Object, Obj)
                   ]

-- collections that map to []
setRepTyp :: Set.Set Name
setRepTyp
    = Set.fromList [ ''Set.Set
                   , ''HashSet.HashSet
                   , ''IntSet.IntSet
                   , ''Vector.Vector
                   , ''[]
                   ]
                            
