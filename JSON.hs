{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, DataKinds, ConstraintKinds, FlexibleInstances, OverlappingInstances, UndecidableInstances, ScopedTypeVariables, GADTs, FlexibleContexts #-}
module JSON where

import NowHs
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
    type Context Rep s m a = (ToJSON s m a, FromJSON s m a)
    toIR = toJSON
    fromIR = fromJSON

class (MonadIO m, Functor m) => ToJSON s m a where
    toJSON :: a -> NowHsT False s Rep m Rep
class (MonadIO m, Functor m) => FromJSON s m a where
    fromJSON :: Rep -> NowHsT False s Rep m (Either String a)

instance (A.ToJSON a, MonadIO m, Functor m) => ToJSON s m a where
    toJSON = return . SimpleJSON . A.toJSON
instance (A.FromJSON a, MonadIO m, Functor m) => FromJSON s m a where
    fromJSON (SimpleJSON r) = case A.fromJSON r of
                                A.Success a -> return $ Right a
                                A.Error err -> return $ Left err
    fromJSON _ = return $ Left "Non-function value expected"

instance (Register fty s Rep m (b -> f)) => ToJSON s m (b -> f) where
    toJSON = phantomProxy (Proxy :: Proxy fty) . functionToJSON
instance (MonadIO m, Functor m, Typeable (b -> f)) => FromJSON s m (b -> f) where
    fromJSON = jsonToFunction

instance (Register ServerType s Rep m (Server s rep m a),
          MonadIO m, Functor m) => ToJSON s m (Server s rep m a) where
    toJSON = phantomProxy (Proxy :: Proxy ServerType) . functionToJSON
instance (MonadIO m, Functor m,
          Typeable (Server s rep m a)) => FromJSON s m (Server s rep m a) where
    fromJSON = jsonToFunction

instance (Register ClientType s Rep m (Client s rep m a),
          MonadIO m, Functor m) => ToJSON s m (Client s rep m a) where
    toJSON = phantomProxy (Proxy :: Proxy ClientType) . functionToJSON
instance (MonadIO m, Functor m,
          Typeable (Client s rep m a)) => FromJSON s m (Client s rep m a) where
    fromJSON = jsonToFunction

functionToJSON :: forall fty s m f. (Register fty s Rep m f, MonadIO m, Functor m) =>
                  f -> PhantomT (NowHsT False s Rep m) fty Rep
functionToJSON f = PhantomT $ do
  fid <- register f :: NowHsT False s Rep m (FunctionID fty)
  return $ case fid of
    ClientID i -> ClientFunction i
    ServerID i -> ServerFunction i

jsonToFunction :: (MonadIO m, Functor m, Typeable f) =>
                  Rep -> NowHsT False s Rep m (Either String f)
jsonToFunction (ClientFunction i) = lookup (ClientID i)
jsonToFunction (ServerFunction i) = lookup (ServerID i)
jsonToFunction _ = return $ Left "Function value expected"


