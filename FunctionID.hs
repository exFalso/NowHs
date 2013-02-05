{-# LANGUAGE DataKinds, KindSignatures, GeneralizedNewtypeDeriving, GADTs, FlexibleContexts #-}
module FunctionID
    ( FunctionID(..)
    , FunctionIDClass(..)
    , FunctionType(..)
    , rawID
    ) where

import IDGen

import Control.Monad.IO.Class
import Control.Applicative
import Control.Concurrent.STM

data FunctionType
    = ClientType | ServerType
      deriving (Show)

data FunctionID fty where
    ClientID :: Int -> FunctionID ClientType
    ServerID :: Int -> FunctionID ServerType

rawID :: FunctionID fty -> Int
rawID (ClientID i) = i
rawID (ServerID i) = i

class FunctionIDClass fty where
    genFunID :: (MonadIO m, Functor m, MonadIDGen Int m) => m (FunctionID fty)
instance FunctionIDClass ServerType where
    genFunID = ServerID <$> (liftIO . atomically =<< genID)
instance FunctionIDClass ClientType where
    genFunID = ClientID <$> (liftIO . atomically =<< genID)
