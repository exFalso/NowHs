{-# LANGUAGE GeneralizedNewtypeDeriving, DataKinds, GADTs, KindSignatures, FlexibleInstances, ConstraintKinds, StandaloneDeriving, FlexibleContexts #-}
module NowHs
    ( NowHsT
    , MonadNowHs(..)
    , AnyFun(..)
    , NowHsEnv(..)
    , embedNoError
    , runNowHsTNoError
    , runNowHsT
    , nowHsError
    , getClientMap
    , getServerMap
    ) where

import IDGen
import Error
import Forkable
import Switch
import FunctionID

import Control.Concurrent.STM
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Data.Dynamic
import qualified Control.Monad.Error as Err
import qualified Data.IntMap as IM

newtype NowHsT err rep m a
    = NowHsT { unNowHs :: SwitchT err (Err.ErrorT Error)
                          (ReaderT NowHsEnv
                           (IDGenT Int m)) a
             }
      deriving (Monad, Functor, MonadIDGen Int, MonadIO)
deriving instance (Forkable m) => Forkable (NowHsT False rep m)
instance (SwitchClass err) => MonadTrans (NowHsT err rep) where
    lift = NowHsT . lift . lift . lift

type ServerMap = IM.IntMap (AnyFun ServerType)

type ClientMap = IM.IntMap (AnyFun ClientType)

data NowHsEnv
     = NowHsEnv { clientFuns :: TVar ClientMap
                , serverFuns :: TVar ServerMap
                }

embedNoError :: (EmbedSwitch err, Monad m) => NowHsT False rep m a -> NowHsT err rep m a
embedNoError (NowHsT m) = NowHsT (embedSwitchT m)

runNowHsTNoError :: (MonadIO m) => NowHsT False rep m a -> m a
runNowHsTNoError n = do
  ac <- liftIO $ newTVarIO IM.empty
  as <- liftIO $ newTVarIO IM.empty
  startIDGenT . runReaderT (runSwitchOffT $ unNowHs n) $ NowHsEnv ac as

runNowHsT :: (MonadIO m) => NowHsT err rep m a -> m (Either Error a)
runNowHsT n = do
  ac <- liftIO $ newTVarIO IM.empty
  as <- liftIO $ newTVarIO IM.empty
  startIDGenT . runReaderT (foldSwitchT Err.runErrorT (liftM Right) $ unNowHs n) $ NowHsEnv ac as

nowHsError :: (Monad m) => Error -> NowHsT True rep m a
nowHsError = NowHsT . OnT . Err.throwError

class (Functor m, MonadIO m, MonadIDGen Int m) => MonadNowHs m where
    getEnv :: m NowHsEnv
    -- liftUm :: um a -> m a

instance (SwitchClass err, Functor m, MonadIO m) => MonadNowHs (NowHsT err rep m) where
    getEnv = NowHsT . lift $ ask
    -- liftUm = lift

getServerMap :: (MonadNowHs m) => m (TVar ServerMap)
getServerMap = serverFuns <$> getEnv

getClientMap :: (MonadNowHs m) => m (TVar ClientMap)
getClientMap = clientFuns <$> getEnv

data AnyFun (fty :: FunctionType) where
    AnyFun :: Typeable f => f -> AnyFun fty

