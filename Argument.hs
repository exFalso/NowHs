{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, DataKinds, ConstraintKinds, FlexibleInstances, UndecidableInstances, ScopedTypeVariables, GADTs, FunctionalDependencies #-}
module Argument where

import IR
import NowHs
import Server
import Client
import Register
import FunctionID
import Phantom

import Control.Monad.IO.Class
import GHC.Prim
import Prelude hiding (lookup)
import Data.Typeable

import qualified Data.Aeson as A

-- The intermediate type that wraps simply convertible values (GenRep)
data Argument rep
    = SimpleArgument rep
    | ServerFunction Int
    | ClientFunction Int
      deriving (Show)

instance (GenRep rep) => IR (Argument rep) m where
    type Context (Argument rep) m a = (ToRep rep m a, FromRep rep m a)
    toIR = toRep
    fromIR = fromRep

-- Generalises to/from
class GenRep rep where
    type CxtTo rep :: * -> Constraint
    type CxtFrom rep :: * -> Constraint
    to :: (CxtTo rep a) => a -> rep
    from :: (CxtFrom rep a) => rep -> Either String a

instance GenRep A.Value where
    type CxtTo A.Value = A.ToJSON
    type CxtFrom A.Value = A.FromJSON
    to = A.toJSON
    from r = case A.fromJSON r of
               A.Success a -> Right a
               A.Error err -> Left err

instance GenRep String where
    type CxtTo String = Show
    type CxtFrom String = Read
    to = show
    from s = case reads s of
               ((a, _) : _) -> return a
               [] -> Left "Failed to Read value"


-- and finally the actual work
class (MonadIO m, Functor m) => ToRep rep m a | m -> rep where
    toRep :: a -> m (Argument rep)
class (MonadIO m, Functor m) => FromRep rep m a where
    fromRep :: (Argument rep) -> m (Either String a)

-- instance (GenRep rep, CxtTo rep a, MonadIO m, Functor m) => ToRep rep m a where
--     toRep a = return . SimpleArgument . to $ a
-- instance (GenRep rep, CxtFrom rep a, MonadIO m, Functor m) => FromRep rep m a where
--     fromRep (SimpleArgument r) = return . from $ r
--     fromRep _ = return $ Left "Non-function value expected"

instance (Register fty m (b -> f), MonadNowHs m rep) => ToRep rep m (b -> f) where
    toRep f = phantomProxy (Proxy :: Proxy fty) . functionToRep $ f
instance (MonadIO m, Functor m, Typeable (b -> f), Side f ~ fty,
          MonadNowHs m rep, RepToFunction fty (b -> f)) => FromRep rep m (b -> f) where
    fromRep r = repToFunction (Proxy :: Proxy fty) r

instance (Register ServerType m (Server s rep um a), MonadNowHs m rep,
          MonadIO m, Functor m) => ToRep rep m (Server s rep um a) where
    toRep s = phantomProxy (Proxy :: Proxy ServerType) . functionToRep $ s
instance (MonadIO m, Functor m, MonadNowHs m rep,
          Typeable (Server s rep um a)) => FromRep rep m (Server s rep um a) where
    fromRep r = repToFunction (Proxy :: Proxy ServerType) r

instance (Register ClientType m (Client rep m a), MonadNowHs m rep,
          MonadIO m, Functor m) => ToRep rep m (Client rep m a) where
    toRep c = phantomProxy (Proxy :: Proxy ClientType) . functionToRep $ c
instance (MonadIO m, Functor m, MonadNowHs m rep,
          Typeable (Client rep m a)) => FromRep rep m (Client rep m a) where
    fromRep r = repToFunction (Proxy :: Proxy ClientType) r
    
functionToRep :: forall fty m f rep. (Register fty m f, MonadIO m, Functor m, MonadNowHs m rep) =>
                  f -> PhantomT m fty (Argument rep)
functionToRep f = PhantomT $ do
  fid <- register f :: m (FunctionID fty)
  return $ case fid of
    ClientID i -> ClientFunction i
    ServerID i -> ServerFunction i

class (Typeable f) => RepToFunction (fty :: FunctionType) f where
    repToFunction :: (MonadIO m, Functor m, MonadNowHs m rep) =>
                     Proxy fty -> Argument rep -> m (Either String f)

instance (Typeable f, Side f ~ ServerType) => RepToFunction ServerType f where
    repToFunction _ (ServerFunction i) = lookup (ServerID i)
    repToFunction _ _ = return $ Left "Server function expected"

instance (Typeable f, Side f ~ ClientType) => RepToFunction ClientType f where
    repToFunction _ (ClientFunction i) = lookup (ClientID i)
    repToFunction _ _ = return $ Left "Client function expected"


