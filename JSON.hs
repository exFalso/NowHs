{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, DataKinds, ConstraintKinds, FlexibleInstances, OverlappingInstances, UndecidableInstances, ScopedTypeVariables, GADTs, FlexibleContexts #-}
module JSON where

import IR
import NowHs
import Server
import Client
import Control.Monad.IO.Class
import Register
import FunctionID
import Phantom

import Prelude hiding (lookup)
import Data.Typeable

import qualified Data.Aeson as A

data JSONArgument
    = SimpleJSON A.Value
    | ServerFunction Int
    | ClientFunction Int

type Rep = JSONArgument

instance IR Rep where
    type Context Rep m a = (ToJSON m a, FromJSON m a)
    toIR = toJSON
    fromIR = fromJSON

class (MonadIO m, Functor m, MonadNowHs m) => ToJSON m a where
    toJSON :: a -> m Rep
class (MonadIO m, Functor m, MonadNowHs m) => FromJSON m a where
    fromJSON :: Rep -> m (Either String a)

instance (A.ToJSON a, MonadIO m, Functor m, MonadNowHs m) => ToJSON m a where
    toJSON = return . SimpleJSON . A.toJSON
instance (A.FromJSON a, MonadIO m, Functor m, MonadNowHs m) => FromJSON m a where
    fromJSON (SimpleJSON r) = case A.fromJSON r of
                                A.Success a -> return $ Right a
                                A.Error err -> return $ Left err
    fromJSON _ = return $ Left "Non-function value expected"

instance (Register fty m (b -> f), MonadNowHs m) => ToJSON m (b -> f) where
    toJSON = phantomProxy (Proxy :: Proxy fty) . functionToJSON
instance (MonadIO m, Functor m, Typeable (b -> f), MonadNowHs m) => FromJSON m (b -> f) where
    fromJSON = jsonToFunction

instance (Register ServerType m (Server rep m a), MonadNowHs m,
          MonadIO m, Functor m) => ToJSON m (Server rep m a) where
    toJSON = phantomProxy (Proxy :: Proxy ServerType) . functionToJSON
instance (MonadIO m, Functor m, MonadNowHs m,
          Typeable (Server rep m a)) => FromJSON m (Server rep m a) where
    fromJSON = jsonToFunction

instance (Register ClientType m (Client rep m a), MonadNowHs m,
          MonadIO m, Functor m) => ToJSON m (Client rep m a) where
    toJSON = phantomProxy (Proxy :: Proxy ClientType) . functionToJSON
instance (MonadIO m, Functor m, MonadNowHs m,
          Typeable (Client rep m a)) => FromJSON m (Client rep m a) where
    fromJSON = jsonToFunction

functionToJSON :: forall fty m f. (Register fty m f, MonadIO m, Functor m, MonadNowHs m) =>
                  f -> PhantomT m fty Rep
functionToJSON f = PhantomT $ do
  fid <- register f :: m (FunctionID fty)
  return $ case fid of
    ClientID i -> ClientFunction i
    ServerID i -> ServerFunction i

jsonToFunction :: (MonadIO m, Functor m, Typeable f, MonadNowHs m) =>
                  Rep -> m (Either String f)
jsonToFunction (ClientFunction i) = lookup (ClientID i)
jsonToFunction (ServerFunction i) = lookup (ServerID i)
jsonToFunction _ = return $ Left "Function value expected"


