{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, DeriveDataTypeable, DefaultSignatures #-}

module NowHs where

import Data.Typeable
import Control.Async
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Reader
import Control.Exception

import Language.Haskell.TH


import qualified Network.WebSockets as WS

data NowHsError
    = NoSuchFunction Name
    | IncorrectNumArgs
    | JSONParseError String
    deriving (Show, Typeable)

instance Error NowHsError
instance Exception NowHsError

newtype NowHsT m a = NowHs (ErrorT NowHsError m a)
                   deriving (Functor, Monad, MonadIO, MonadTrans,
                             MonadError NowHsError)

type Prot = WS.Hybi00

type NowHs = NowHsT (AsyncT (WS.WebSockets Prot))

class (Monad m) => MonadNowHs m where
    liftNowHs :: NowHs a -> m a
    default liftNowHs :: (MonadTrans t, MonadNowHs m, Monad (t m)) => NowHs a -> t m a
    liftNowHs = lift . liftNowHs

instance MonadNowHs NowHs where
    liftNowHs = id

instance (MonadNowHs m) => MonadNowHs (StateT s m)
instance (MonadNowHs m) => MonadNowHs (ReaderT r m)


    