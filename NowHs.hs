{-# LANGUAGE GeneralizedNewtypeDeriving, DataKinds, GADTs, KindSignatures, MultiParamTypeClasses, FlexibleInstances, ConstraintKinds, UndecidableInstances, TypeFamilies, StandaloneDeriving, RankNTypes, FlexibleContexts, FunctionalDependencies #-}
module NowHs where

import IDGen
import Error
import FunctionID
import Forkable
import Switch

import GHC.Prim
import Control.Concurrent
import Control.Concurrent.STM
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Data.Dynamic
import qualified Control.Monad.Error as Err
import qualified Data.IntMap as IM

class IR m rep | m -> rep where
    type Context rep m a :: Constraint
    toIR :: (Context rep m a, MonadIO m) => a -> m rep
    fromIR :: (Context rep m a, MonadIO m) => rep -> m (Either String a)

newtype NowHsT err s m a
    = NowHsT { unNowHs :: SwitchT err (Err.ErrorT Error)
                          (ReaderT (NowHsEnv s)
                           (IDGenT Int m)) a
             }
      deriving (Monad, Functor, MonadIDGen Int, MonadIO)
deriving instance (Forkable m) => Forkable (NowHsT False s m)

type ServerMap = IM.IntMap AnyServer

type ClientMap = IM.IntMap AnyClient

data NowHsEnv s
     = NowHsEnv { clientFuns :: TVar ClientMap
                , serverFuns :: TVar ServerMap
                , sharedState :: TVar s
                }

newtype Server s m a = Server { unServer :: (NowHsT False s m) a
                                  }
    deriving (Monad, Functor, MonadIO, Forkable)

data Client rep m a = Client (m [rep]) (FunctionID ClientType)

embedNoError :: (EmbedSwitch err, Monad m) => NowHsT False s m a -> NowHsT err s m a
embedNoError (NowHsT m) = NowHsT (embedSwitchT m)

forkServer :: (Monad m, Forkable m, EmbedSwitch err) => Server s m () -> NowHsT err s m ThreadId
forkServer = embedNoError . fork . unServer

runNowHsTNoError :: (MonadIO m) => TVar s -> NowHsT False s m a -> m a
runNowHsTNoError tvar n = do
  ac <- liftIO $ newTVarIO IM.empty
  as <- liftIO $ newTVarIO IM.empty
  startIDGenT . runReaderT (runSwitchOffT $ unNowHs n) $ NowHsEnv ac as tvar

runNowHsT :: (MonadIO m) => TVar s -> NowHsT err s m a -> m (Either Error a)
runNowHsT tvar n = do
  ac <- liftIO $ newTVarIO IM.empty
  as <- liftIO $ newTVarIO IM.empty
  startIDGenT . runReaderT (foldSwitchT Err.runErrorT (liftM Right) $ unNowHs n) $ NowHsEnv ac as tvar

nowHsError :: (Monad m) => Error -> NowHsT True s m a
nowHsError = NowHsT . OnT . Err.throwError

getServerMap :: (Monad m, Functor m) =>
                NowHsT False s m (TVar ServerMap)
getServerMap = serverFuns <$> getEnv

getClientMap :: (Monad m, Functor m) =>
                NowHsT False s m (TVar ClientMap)
getClientMap = clientFuns <$> getEnv

getEnv :: (Monad m) => NowHsT False s m (NowHsEnv s)
getEnv = NowHsT . lift $ ask

data AnyServer where
    AnyServer :: Typeable f => f -> AnyServer

data AnyClient where
    AnyClient :: Typeable f => f -> AnyClient

