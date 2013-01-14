{-# LANGUAGE DataKinds, FlexibleInstances, GADTs, FlexibleContexts #-}
module Forkable where

import Switch

import Control.Concurrent
import Control.Monad.Reader

class (Monad m) => Forkable m where
    fork :: m () -> m ThreadId

instance (Forkable m) => Forkable (ReaderT r m) where
    fork m = lift . fork . runReaderT m =<< ask

instance Forkable IO where
    fork = forkIO

instance (Forkable m, Monad m, MonadTrans t, Monad (t m)) =>
    Forkable (SwitchT False t m) where
    fork (OffT m) = OffT $ fork m
