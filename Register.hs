{-# LANGUAGE DataKinds, MultiParamTypeClasses, FlexibleInstances, GADTs, FunctionalDependencies, TypeFamilies, UndecidableInstances, ScopedTypeVariables #-}
module Register ( Register(..)
                , Side
                ) where

import FunctionID
import NowHs
import Client
import Server
import Proxy

import Control.Applicative
import Data.Dynamic
import Control.Concurrent.STM
import Control.Monad.IO.Class
import qualified Data.IntMap as IM

type family Side f :: FunctionType
type instance Side (Client rep m a) = ClientType
type instance Side (Server s rep m a) = ServerType
type instance Side (a -> f) = Side f

class (MonadIO m, Functor m) => Register fty m f | f -> fty where
    register :: f -> m (FunctionID fty)
    lookup :: FunctionID fty -> m (Either String f)

instance (fty ~ Side f, RegisterFun fty m, Typeable f) => Register fty m f where
    register = registerFun (Proxy :: Proxy fty)
    lookup = lookupFun (Proxy :: Proxy fty)

class (MonadIO m, Functor m) => RegisterFun fty m where
    registerFun :: (Typeable f) => Proxy fty -> f -> m (FunctionID fty)
    lookupFun :: (Typeable f) => Proxy fty -> FunctionID fty -> m (Either String f)

instance (MonadNowHs m rep) => RegisterFun ServerType m where
    registerFun _ f = do
      sid <- genFunID
      registerServer sid f
      return sid
    lookupFun _ (ServerID sid) = do
      sm <- getServerMap
      mf <- IM.lookup sid <$> (liftIO . atomically . readTVar $ sm)
      return $ case mf of
                 Nothing -> Left "Server function ID not found"
                 Just (AnyFun g) ->
                     case cast g of
                       Nothing -> Left "Failed to cast functiontype"
                       Just f -> return f

instance (MonadNowHs m rep) => RegisterFun ClientType m where
    registerFun _ f = do
      cid <- genFunID
      registerClient cid f
      return cid
    lookupFun _ (ClientID sid) = do
      sm <- getClientMap
      mf <- IM.lookup sid <$> (liftIO . atomically . readTVar $ sm)
      return $ case mf of
                 Nothing -> Left "Client function ID not found"
                 Just (AnyFun g) ->
                     case cast g of
                       Nothing -> Left "Failed to cast functiontype"
                       Just f -> return f

registerFunction :: (Typeable f, MonadIO m) => Int -> f -> TVar (IM.IntMap (AnyFun fty)) -> m ()
registerFunction fid f sm = liftIO . atomically . modifyTVar sm . IM.insert fid $ AnyFun f

registerServer :: (Typeable t, MonadNowHs m rep) => FunctionID ServerType -> t -> m ()
registerServer (ServerID sid) f = getServerMap >>= registerFunction sid f

registerClient :: (Typeable t, MonadNowHs m rep) => FunctionID ClientType -> t -> m ()
registerClient (ClientID cid) f = getClientMap >>= registerFunction cid f
