{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, DefaultSignatures, FlexibleContexts #-}
module IDGen
    ( IDGenT
    , IDGen
    , MonadIDGen(..)
    , runIDGenT
    , startIDGenT
    ) where

import Forkable
import Switch

import Control.Concurrent.STM
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.Identity
import Control.Applicative

newtype IDGenT i m a
    = IDGenT (ReaderT (TVar i) m a)
      deriving (Monad, Functor, Applicative, MonadIO, Forkable, MonadTrans)

type IDGen i = IDGenT i Identity

class (Monad m) => MonadIDGen i m where
    genID :: (Num i) => m (STM i)
    default genID :: (MonadTrans t, MonadIDGen i m, Num i) => t m (STM i)
    genID = lift genID

instance (Monad m) => MonadIDGen i (IDGenT i m) where
    genID = IDGenT . ReaderT $ \tvar -> return $ do
              v <- readTVar tvar
              modifyTVar tvar (+ 1)
              return v

instance (MonadIDGen i m) => MonadIDGen i (ReaderT r m) where
instance (MonadIDGen i m) => MonadIDGen i (StateT s m) where
instance (MonadIDGen i m, Error e) => MonadIDGen i (ErrorT e m) where
instance (MonadIDGen i m, SwitchClass on, MonadTrans t, Monad (t m)) => MonadIDGen i (SwitchT on t m)

runIDGenT :: (Monad m, Num i) => TVar i -> IDGenT i m a -> m a
runIDGenT tvar (IDGenT r) = runReaderT r tvar

startIDGenT :: (Num i, MonadIO m) => IDGenT i m a -> m a
startIDGenT idg = do
  tvar <- liftIO (newTVarIO 0)
  runIDGenT tvar idg
