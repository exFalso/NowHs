{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, DefaultSignatures, PolyKinds, TypeOperators #-}
module Phantom
    ( module Proxy
    , PhantomT(..)
    , MonadPhantom(..)
    , runPhantom
    , phantomProxy
    , proxy
    ) where

import Proxy

import Control.Applicative
import Control.Monad.Identity

newtype PhantomT m p a = PhantomT { runPhantomT :: m a }

type Phantom = PhantomT Identity

runPhantom :: Phantom p a -> a
runPhantom = runIdentity . runPhantomT

phantomProxy :: Proxy p -> PhantomT m p a -> m a
phantomProxy _ = runPhantomT

proxy :: (MonadPhantom m, Monad (m p)) => m p (Proxy p)
proxy = return Proxy

class MonadPhantom m where
    phantom :: m p0 a -> m p1 a

instance (Monad m) => MonadPhantom (PhantomT m) where
    phantom (PhantomT m) = PhantomT m

deriving instance (Functor m) => Functor (PhantomT m p)
deriving instance (Monad m) => Monad (PhantomT m p)
deriving instance (Applicative m) => Applicative (PhantomT m p)
