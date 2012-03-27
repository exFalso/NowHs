{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, DeriveDataTypeable #-}

module NowHs where

import Data.Typeable
import Control.Monad.State
import Control.Monad.Error
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

type NowHs = NowHsT (WS.WebSockets Prot)

class (Monad m) => MonadNowHs m where
    liftNowHs :: NowHs a -> m a

instance MonadNowHs (NowHsT (WS.WebSockets Prot)) where
    liftNowHs = id
