{-# LANGUAGE GADTs, DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module Switch where

import Control.Monad.Trans

data SwitchT on t m a where
    OnT :: t m a -> SwitchT True t m a
    OffT :: m a -> SwitchT False t m a

data SwitchM on m a where
    OnM :: m a -> SwitchM True m a
    OffM :: a -> SwitchM False m a

runSwitchOnT :: SwitchT True t m a -> t m a
runSwitchOnT (OnT tma) = tma

runSwitchOffT :: SwitchT False t m a -> m a
runSwitchOffT (OffT ma) = ma

runSwitchOnM :: SwitchM True m a -> m a
runSwitchOnM (OnM ma) = ma

runSwitchOffM :: SwitchM False m a -> a
runSwitchOffM (OffM a) = a

foldSwitchT :: (t m a -> c) -> (m a -> c) -> SwitchT on t m a -> c
foldSwitchT f _ (OnT tma) = f tma
foldSwitchT _ g (OffT ma) = g ma

foldSwitchM :: (m a -> c) -> (a -> c) -> SwitchM on m a -> c
foldSwitchM f _ (OnM tma) = f tma
foldSwitchM _ g (OffM ma) = g ma

class EmbedSwitch on where
    embedSwitchT :: (Monad m, MonadTrans t) => SwitchT False t m a -> SwitchT on t m a
    embedSwitchM :: (Monad m) => SwitchM False m a -> SwitchM on m a
instance EmbedSwitch True where
    embedSwitchT (OffT m) = OnT $ lift m
    embedSwitchM (OffM a) = OnM $ return a
instance EmbedSwitch False where
    embedSwitchT = id
    embedSwitchM = id

class SwitchClass on where
    switchReturnT :: (Monad m, MonadTrans t) => a -> SwitchT on t m a
    switchBindT :: (Monad m, Monad (t m)) => SwitchT on t m a -> (a -> SwitchT on t m b) ->
                  SwitchT on t m b
    switchLiftT :: (Monad m, MonadTrans t) => m a -> SwitchT on t m a

    switchReturnM :: (Monad m) => a -> SwitchM on m a
    switchBindM :: (Monad m) => SwitchM on m a -> (a -> SwitchM on m b) ->
                  SwitchM on m b

instance SwitchClass True where
    switchReturnT = OnT . lift . return
    switchBindT (OnT tma) f = OnT (tma >>= runSwitchOnT . f)
    switchLiftT = OnT . lift

    switchReturnM = OnM . return
    switchBindM (OnM ma) f = OnM (ma >>= runSwitchOnM . f)

instance SwitchClass False where
    switchReturnT = OffT . return
    switchBindT (OffT ma) f = OffT (ma >>= runSwitchOffT . f)
    switchLiftT = OffT

    switchReturnM = OffM
    switchBindM (OffM a) f = OffM (runSwitchOffM . f $ a)

instance (Functor (t m), Functor m) => Functor (SwitchT on t m) where
    fmap f (OnT tma) = OnT $ fmap f tma
    fmap f (OffT ma) = OffT $ fmap f ma
instance (SwitchClass on, Monad m, MonadTrans t, Monad (t m)) => Monad (SwitchT on t m) where
    return = switchReturnT
    (>>=) = switchBindT
instance (SwitchClass on, MonadTrans t) => MonadTrans (SwitchT on t) where
    lift = switchLiftT
instance (MonadIO m, MonadTrans t, SwitchClass on, Monad (t m)) => MonadIO (SwitchT on t m) where
    liftIO = lift . liftIO

instance MonadTrans (SwitchM True) where
    lift = OnM

instance (Functor m) => Functor (SwitchM on m) where
    fmap f (OnM ma) = OnM $ fmap f ma
    fmap f (OffM a) = OffM $ f a
instance (SwitchClass on, Monad m) => Monad (SwitchM on m) where
    return = switchReturnM
    (>>=) = switchBindM
