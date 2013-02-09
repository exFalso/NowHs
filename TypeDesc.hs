{-# LANGUAGE OverlappingInstances, ScopedTypeVariables, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module TypeDesc where

import FunctionID
import Data.Typeable
import Phantom
import Client
import Server

import Control.Applicative

data FunctionDesc
    = FunctionDesc FunctionType [TypeDesc] TypeDesc -- type, arguments, return type
      deriving (Show)

data TypeDesc
    = TypeSimple TypeRep
    | TypeFunction FunctionDesc
      deriving (Show)

class GetFunctionDesc f where
    getFunctionDesc :: Phantom f FunctionDesc
instance (GetTypeDesc a, GetFunctionDesc f) => GetFunctionDesc (a -> f) where
    getFunctionDesc = do
      p <- phantom $ (getTypeDesc :: Phantom a TypeDesc)
      FunctionDesc fty ps r <- phantom $ (getFunctionDesc :: Phantom f FunctionDesc)
      return $ FunctionDesc fty (p : ps) r
instance (GetTypeDesc r) => GetFunctionDesc (Client rep m r) where
    getFunctionDesc = do
      p <- phantom $ (getTypeDesc :: Phantom r TypeDesc)
      return $ FunctionDesc ClientType [] p
instance (GetTypeDesc r) => GetFunctionDesc (Server s rep m r) where
    getFunctionDesc = do
      p <- phantom $ (getTypeDesc :: Phantom r TypeDesc)
      return $ FunctionDesc ServerType [] p

class GetTypeDesc f where
    getTypeDesc :: Phantom f TypeDesc
instance (Typeable a) => GetTypeDesc a where
    getTypeDesc = return . TypeSimple $ typeOf (undefined :: a)
instance (GetFunctionDesc (a -> f)) => GetTypeDesc (a -> f) where
    getTypeDesc = TypeFunction <$> getFunctionDesc
instance (GetFunctionDesc (Client rep m r)) => GetTypeDesc (Client rep m r) where
    getTypeDesc = TypeFunction <$> getFunctionDesc
instance (GetFunctionDesc (Server s rep m r)) => GetTypeDesc (Server s rep m r) where
    getTypeDesc = TypeFunction <$> getFunctionDesc
