{-# LANGUAGE DataKinds, MultiParamTypeClasses, FlexibleInstances, GADTs, FunctionalDependencies #-}
module Register ( Register(..)
                ) where

import FunctionID
import NowHs

import Control.Applicative
import Data.Dynamic
import Control.Concurrent.STM
import Control.Monad.IO.Class
import qualified Data.IntMap as IM

class (MonadIO m, Functor m) => Register fty m f where
    register :: f -> m (FunctionID fty)
    lookup :: FunctionID fty -> m (Either String f)

instance (MonadNowHs m, Typeable f) => Register ServerType m f where
    register f = do
      sid <- genFunID
      registerServer sid f
      return sid
    lookup (ServerID sid) = do
      sm <- getServerMap
      mf <- IM.lookup sid <$> (liftIO . atomically . readTVar $ sm)
      return $ case mf of
                 Nothing -> Left "Server function ID not found"
                 Just (AnyFun g) ->
                     case cast g of
                       Nothing -> Left "Failed to cast functiontype"
                       Just f -> return f

instance (MonadNowHs m, Typeable f) =>
    Register ClientType m f where
    register f = do
      cid <- genFunID
      registerClient cid f
      return cid
    lookup (ClientID sid) = do
      sm <- getClientMap
      mf <- IM.lookup sid <$> (liftIO . atomically . readTVar $ sm)
      return $ case mf of
                 Nothing -> Left "Client function ID not found"
                 Just (AnyFun g) ->
                     case cast g of
                       Nothing -> Left "Failed to cast functiontype"
                       Just f -> return f

registerFun :: (Typeable f, MonadIO m) => Int -> f -> TVar (IM.IntMap (AnyFun fty)) -> m ()
registerFun fid f sm = liftIO . atomically . modifyTVar sm . IM.insert fid $ AnyFun f

registerServer :: (Typeable t, MonadNowHs m) => FunctionID ServerType -> t -> m ()
registerServer (ServerID sid) f = getServerMap >>= registerFun sid f

registerClient :: (Typeable t, MonadNowHs m) => FunctionID ClientType -> t -> m ()
registerClient (ClientID cid) f = getClientMap >>= registerFun cid f
