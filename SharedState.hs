{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module SharedState where

import Control.Concurrent.STM
import Control.Monad.Reader

class (Monad m) => MonadSharedState s m where
    putShared :: s -> m (STM ())
    getShared :: m (STM s)
    getsShared :: (s -> a) -> m (STM a)
    getsShared f = (liftM . liftM) f getShared

instance (Monad m) => MonadSharedState s (ReaderT (TVar s) m) where
    putShared s = liftM2 writeTVar ask (return s)
    getShared = liftM readTVar ask
