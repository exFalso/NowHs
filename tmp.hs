{-# LANGUAGE DefaultSignatures, FlexibleContexts, KindSignatures, MultiParamTypeClasses, FlexibleInstances, DeriveGeneric #-}

module Tmp where

import Data.Aeson
import Control.Monad.Error

data NowHs a = NOWHS

instance Monad NowHs

instance MonadError SomeError NowHs

data SomeError = IncorrectNumArgs | ParseError String

class Callable f where
    call :: f -> [Value] -> NowHs Value

instance (FromJSON a, Callable f) => Callable (a -> f) where
    call f (v : vs) = case
        fromJSON v of
            Success a -> call (f a) vs
            Error e -> throwError (ParseError e)
    call _ [] =
        throwError IncorrectNumArgs

instance (ToJSON a) => Callable (NowHs a) where
    call f [] = liftM toJSON f
    call _ _  = throwError IncorrectNumArgs
