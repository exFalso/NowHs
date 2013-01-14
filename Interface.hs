{-# LANGUAGE ScopedTypeVariables, TypeOperators, DataKinds, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, UndecidableInstances, GADTs, GeneralizedNewtypeDeriving #-}
module Interface where

import NewClient
import Util
import FunctionID
import DynStat
import Forkable

import Control.Applicative
import Control.Monad.State
import GHC.Generics

import qualified Data.IntMap as IM

-- function id -> function name
type Interface = IM.IntMap String

newtype DynIfaceT dyn m a = DynIfaceT { unDynIface :: DynStatT dyn Interface m a
                                      }
    deriving (Functor, Monad, MonadIO, Forkable)

type IdSel = StateT (IM.IntMap String)
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
instance (RegisterIfaceFunction m a, HasSelector g True, Selector s) =>
    RegisterIfaceGeneric m (S1 s (K1 p a)) where
        registerIface = do
          (ClientID rawFid, f) <- lift (registerIfaceFun :: m (FunctionID ClientType, a))
          modify $ IM.insert rawFid (selName (undefined :: S1 s (K1 p a) x))
          return $ M1 (K1 f)

class (Monad m, Functor m) => RegisterIfaceFunction m a where
    registerIfaceFun :: m (FunctionID ClientType, a)

instance (NewClient m rep a) => RegisterIfaceFunction m a where
    registerIfaceFun = newClient
