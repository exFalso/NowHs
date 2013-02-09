{-# LANGUAGE GeneralizedNewtypeDeriving, DataKinds, DeriveDataTypeable, StandaloneDeriving, ScopedTypeVariables #-}
module Server where

import NowHs
import Forkable

import Data.Typeable
import Control.Monad.IO.Class
import Control.Concurrent

newtype Server s rep m a = Server { unServer :: (NowHsT False rep m) a
                              }
    deriving (Monad, Functor, MonadIO, Forkable)

serverTyCon :: TyCon
serverTyCon = mkTyCon3 "Network.NowHs" "Server" "Server"

instance (Typeable s, Typeable rep, Typeable1 m, Typeable a) => Typeable (Server s rep m a) where
    typeOf _
        = mkTyConApp serverTyCon
          [ typeOf (undefined :: s)
          , typeOf (undefined :: rep)
          , typeOf1 (undefined :: m x)
          , typeOf (undefined :: a)
          ]

instance (Typeable s, Typeable rep, Typeable1 m) => Typeable1 (Server s rep m) where
    typeOf1 _
        = mkTyConApp serverTyCon
          [ typeOf (undefined :: s)
          , typeOf (undefined :: rep)
          , typeOf1 (undefined :: m x)
          ]

forkServer :: (Monad m, Forkable m) => Server s rep m () -> NowHsT False rep m ThreadId
forkServer = fork . unServer
