{-# LANGUAGE ScopedTypeVariables, TypeOperators, DataKinds, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances, GADTs, GeneralizedNewtypeDeriving, FunctionalDependencies, KindSignatures #-}
module Interface where

import Client
import Util
import FunctionID
import DynStat
import Forkable
import Phantom
import TypeDesc

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import GHC.Generics

import qualified Data.IntMap as IM

-- function id -> function name
type Interface = IM.IntMap String

newtype DynIfaceT dyn m a = DynIfaceT { unDynIface :: DynStatT dyn Interface m a
                                      }
    deriving (Functor, Monad, MonadIO, MonadTrans, Forkable)


newtype IfaceT iface m a
    = IfaceT { unIfaceT :: ReaderT iface m a }
    deriving (Functor, Monad, MonadIO, MonadTrans, Forkable)

type Binding fty = [(Int, FunDesc fty)]

runIfaceT :: (RegisterInterface m iface) =>
             (Binding ClientType -> IfaceT iface m a) -> m a
runIfaceT f = do
  (iface, clientBinding) <- runWriterT registerInterface
  runReaderT (unIfaceT $ f clientBinding) iface

class (Monad m) => MonadIface iface m | m -> iface where
    getIface :: m iface
instance (Monad m) => MonadIface iface (IfaceT iface m) where
    getIface = IfaceT ask

-- class (Monad m) => Call m f g where
--     callM :: m f -> g
-- instance (Call m f g) => Call m (a -> f) (a -> g) where
--     callM maf a = callM $ liftM ($ a) maf
-- instance (Monad m) => Call m (Client rep m a) (m (Client rep m a)) where
--     callM = id

-- call :: forall iface m f g. (MonadIface iface m, Call m f g) => (iface -> f) -> g
-- call f = callM $ (liftM f getIface :: m f)

data FunDesc (fty :: FunctionType)
    = FunDesc { funName :: String
              , funDesc :: FunctionDesc
              }
      deriving (Show)

type IdSel = WriterT (Binding ClientType)
-- the map is id -> selector
class (Monad m, Functor m, Generic a) => RegisterInterface m a where
    registerInterface :: IdSel m a

instance (Generic a, RegisterIfaceGeneric m (Rep a)) => RegisterInterface m a where
    registerInterface = to <$> registerIface

class (Monad m, Functor m) => RegisterIfaceGeneric m a where
    registerIface :: IdSel m (a x)

instance (RegisterIfaceGeneric m a) => RegisterIfaceGeneric m (D1 d (C1 c a)) where
    registerIface = M1 . M1 <$> registerIface
instance (RegisterIfaceGeneric m a, RegisterIfaceGeneric m b) => RegisterIfaceGeneric m (a :*: b) where
    registerIface = liftM2 (:*:) registerIface registerIface
instance (RegisterIfaceFunction m a, HasSelector g True, Selector s, GetFunctionDesc a) =>
    RegisterIfaceGeneric m (S1 s (K1 p a)) where
        registerIface = do
          (ClientID rawFid, f) <- lift (registerIfaceFun :: m (FunctionID ClientType, a))
          let functionDesc = phantomProxy (Proxy :: Proxy a) getFunctionDesc
              functionName = selName (undefined :: S1 s (K1 p a) x)
          tell $ [(rawFid, FunDesc functionName functionDesc)]
          return $ M1 (K1 f)

class (Monad m, Functor m) => RegisterIfaceFunction m a where
    registerIfaceFun :: m (FunctionID ClientType, a)

instance (NewClient m rep a) => RegisterIfaceFunction m a where
    registerIfaceFun = newClient
