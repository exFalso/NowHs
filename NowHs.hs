{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}

module NowHs where

import Data.Typeable
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Reader
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM

import qualified Network.WebSockets as WS

data NowHsError
    = NoSuchFunction String
    | IncorrectNumArgs
    | JSONParseError String
    deriving (Show, Typeable)

instance Error NowHsError
instance Exception NowHsError

newtype NowHsT s m a = NowHs { unNowHs :: ReaderT (TVar s) (ErrorT NowHsError m) a }
                     deriving (Functor, Monad, MonadIO,
                               MonadError NowHsError)

instance MonadTrans (NowHsT s) where
	lift = NowHs . lift . lift

mapNowHsT :: (m (Either NowHsError a) -> n (Either NowHsError b)) -> NowHsT s m a -> NowHsT s n b
mapNowHsT f = NowHs . mapReaderT (mapErrorT f) . unNowHs

forkNowHs :: MonadIO m => NowHs s () -> NowHsT s m ThreadId
forkNowHs now= do
    let mapping io = do
        liftIO $ fmap Right $ forkIO (fmap (either throw id) io)
    tid <- mapNowHsT mapping now
    return tid

type Prot = WS.Hybi00


type NowHs s = NowHsT s IO

type NowHsWs s = NowHsT s (WS.WebSockets Prot)

instance (MonadIO m) => MonadState s (NowHsT s m) where
    put aasd = do
        tv <- NowHs ask
        liftIO . atomically $ writeTVar tv aasd
    get = do
        tv <- NowHs ask
        liftIO . atomically $ readTVar tv


--class (Monad m) => MonadNowHs m where
--    liftNowHs :: NowHs a -> m a

--instance MonadNowHs (NowHsT (WS.WebSockets Prot)) where
--    liftNowHs = id
