{-# LANGUAGE DataKinds, GADTs, GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving, FunctionalDependencies #-}
module DynStat
    ( DynStatT(..)
    , MonadDyn(..)
    , dynPut
    , dynModify
    ) where

import Switch
import Forkable

import Control.Monad.Reader
import Control.Concurrent.STM

newtype DynStatT dyn r m a = DynStatT { unDynStat :: ReaderT (SwitchM dyn TVar r) m a
                                      }
    deriving (Functor, Monad, MonadIO, Forkable, MonadTrans)

instance (MonadIO m) => MonadReader r (DynStatT True r m) where
    ask = DynStatT . ReaderT $ \(OnM tvar) -> liftIO . atomically . readTVar $ tvar
    local f (DynStatT (ReaderT gr))
        = DynStatT . ReaderT $ \(OnM tvar) -> do
            a <- liftIO . atomically . readTVar $ tvar
            na <- liftIO . newTVarIO $ f a
            gr (OnM na)

instance (Monad m) => MonadReader r (DynStatT False r m) where
    ask = DynStatT . ReaderT $ return . runSwitchOffM
    local f (DynStatT (ReaderT gr))
        = DynStatT . ReaderT $ \(OffM a) -> gr . OffM $ f a

class (Monad m) => MonadDyn r m | m -> r where
    dynTVar :: m (TVar r)

dynPut :: (MonadIO m, MonadDyn t m) => t -> m ()
dynPut = dynF writeTVar

dynModify :: (MonadIO m, MonadDyn r m) => (r -> r) -> m ()
dynModify = dynF modifyTVar

dynF :: (MonadIO m, MonadDyn r m) => (TVar r -> t -> STM b) -> t -> m b
dynF f a = dynTVar >>= \tvar -> liftIO . atomically $ f tvar a

instance (Monad m) => MonadDyn r (DynStatT True r m) where
    dynTVar = DynStatT $ return . runSwitchOnM =<< ask
