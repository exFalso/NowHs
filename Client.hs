{-# LANGUAGE DataKinds, MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, FlexibleInstances, ConstraintKinds, UndecidableInstances #-}
module Client where

import IDGen
import FunctionID
import IR

import Control.Monad
import Control.Monad.IO.Class

data Client rep m a = Client (m [rep]) (FunctionID ClientType)

newClient :: (NewClient m rep f, Monad m, Functor m) => m (FunctionID ClientType, f)
newClient = do
  (fid, f) <- newClient'
  return (fid, f id)

class (MonadIO m, Functor m, IR rep m) => NewClient m rep f | m -> rep where
    newClient' :: m (FunctionID ClientType, (m [rep] -> m [rep]) -> f)

instance (MonadIO m, Functor m, MonadIDGen Int m, IR rep m) =>
    NewClient m rep (Client rep m a) where
    newClient' = do
      fid <- genFunID
      return $ (fid, \f -> Client (f $ return []) fid)

instance (Context rep m a, IR rep m, NewClient m rep f, MonadIO m) =>
    NewClient m rep (a -> f) where
    newClient' = do
      (fid, g) <- newClient'
      return (fid, \f a -> g (liftM2 (:) (toIR a) . f))
