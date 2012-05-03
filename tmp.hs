{-# LANGUAGE DeriveGeneric, ExistentialQuantification, MultiParamTypeClasses, OverlappingInstances, FlexibleInstances, UndecidableInstances, TypeFamilies, DataKinds, PolyKinds, EmptyDataDecls, FunctionalDependencies, ScopedTypeVariables, GeneralizedNewtypeDeriving, ConstraintKinds #-}

module Tmp where

import JSON hiding (Proxy)
import Data.Dynamic
import qualified Data.Aeson as Aeson
import qualified Data.Set as Set
import qualified Data.IntMap as Map
import Control.Monad.State
import Control.Monad.Error
import Internal.Internal

type FunctionName = String

data NowHs a b
data Function
data AnySchema



-- asyncClient :: Client a -> Server ()

-- syncClient :: Client a -> Server a

-- doClientAdd :: (Int -> Int -> Client Int) -> Server Int
-- doClientAdd clAdd = syncClient (clAdd 1 2)

-- handleAddResult :: Int -> Server ()

-- doClientAsyncAdd :: (Int -> Int -> (Int -> Server ()) -> Client ()) -> Server ()
-- doClientAsyncAdd clAdd = asyncClient $ clAdd 1 2 handleAddResult

data Proxy k = Proxy

class Schema a

data Interface s
    = Interface { interfaceInternal :: (FunctionName, [Aeson.Value]) -> NowHs s Aeson.Value
                , interfaceExternal :: ([Function], Set.Set AnySchema) }

hey :: Int -> Int -> Server Int
hey a b = return (a + b)

lol :: (Int -> Int -> Server Int) -> Server Int
lol f = f 1 2

hah :: ((Int -> Int -> Server Int) -> Server Int) -> Server Int
hah f = f hey

lal :: (((Int -> Server Int) -> Server Int) -> Server Int) -> Server Int
lal = lal

whatever :: (Int -> Client Int) -> Server Int
whatever = undefined

whatever2 :: Server Int
whatever2 = undefined

main :: IO ()
main = do
    let internalState = InternalState (Map.fromList [ (0, ServerFunCall hey)
                                                    , (1, ServerFunCall hah)
                                                    , (2, ServerFunCall whatever)])
    print . flip evalState internalState . runErrorT . unInternal $ do
        callServer lol [toJSON (0 :: Int)]

-- data ServerFunCxt cxt = forall a. cxt a => ServerFunCxt a
-- type ServerFunDyn = ServerFunCxt ServerCallDyn

data CallContext 
    = CallContext { inArguments :: [Aeson.Value]
                  , clientArgs :: [Aeson.Value]
                  , serverArgs :: [Dynamic] }


