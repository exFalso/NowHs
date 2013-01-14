{-# LANGUAGE KindSignatures, DataKinds, NamedFieldPuns, MultiParamTypeClasses, FlexibleInstances, ConstraintKinds, UndecidableInstances, GADTs, ScopedTypeVariables, TypeFamilies, FunctionalDependencies #-}
module Register where

import FunctionID
import NowHs

import Control.Applicative
import Data.Dynamic
import Control.Concurrent.STM
import Control.Monad.IO.Class
import qualified Data.IntMap as IM


class (MonadIO m, Functor m) => Register fty s m f where
    register :: f -> NowHsT False s m (FunctionID fty)
    lookup :: FunctionID fty -> NowHsT False s m (Either String f)

instance (Typeable f, MonadIO m, Functor m) => Register ServerType s m f where
    register f = do
      sid <- genFunID
      registerServer sid f
      return sid
    lookup (ServerID sid) = do
      sm <- getServerMap
      mf <- IM.lookup sid <$> (liftIO . atomically . readTVar $ sm)
      return $ case mf of
                 Nothing -> Left "Server function ID not found"
                 Just (AnyServer g) ->
                     case cast g of
                       Nothing -> Left "Failed to cast functiontype"
                       Just f -> return f

instance (MonadIO m, Functor m, Typeable f) =>
    Register ClientType s m f where
    register f = do
      cid <- genFunID
      registerClient cid f
      return cid
    lookup (ClientID sid) = do
      sm <- getClientMap
      mf <- IM.lookup sid <$> (liftIO . atomically . readTVar $ sm)
      return $ case mf of
                 Nothing -> Left "Client function ID not found"
                 Just (AnyClient g) ->
                     case cast g of
                       Nothing -> Left "Failed to cast functiontype"
                       Just f -> return f

registerServer :: (MonadIO m, Functor m, Typeable f) =>
                  FunctionID ServerType -> f -> NowHsT False s m ()
registerServer (ServerID cid) f = do
  sm <- getServerMap
  liftIO . atomically . modifyTVar sm . IM.insert cid $ AnyServer f

registerClient :: (MonadIO m, Functor m, Typeable f) =>
                  FunctionID ClientType -> f -> NowHsT False s m ()
registerClient (ClientID cid) f = do
  cm <- getClientMap
  liftIO . atomically . modifyTVar cm . IM.insert cid $ AnyClient f

