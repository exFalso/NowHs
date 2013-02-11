{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, ScopedTypeVariables, DeriveGeneric, NamedFieldPuns #-}
module Try where

import IR
import Argument
import qualified NowHs as N
import qualified Server as S
import qualified Data.Aeson as A
import Interface
import Client
import FunctionID

import Prelude hiding (lookup)
import Data.Typeable
import GHC.Generics

type NowHs = N.NowHsT False A.Value IO
type Server = S.Server () A.Value IO
type Cl = Client A.Value IO

data Iface
    = Iface { echo :: Int -> Cl Integer
            }
      deriving (Generic)

hello :: IfaceT Iface Server (Cl Integer)
hello = do
  iface <- getIface
  return $ echo iface 1

asd :: Server (Binding ClientType)
asd = runIfaceT (\b -> do
                   _ <- hello
                   return b)

now :: NowHs (Argument A.Value)
now = do
  toIR addInt
  toIR someInt

addInt :: Int -> Int -> Server Int
addInt a b = return (a + b)

someInt :: Server Int
someInt = return 0

ping :: (A.ToJSON a, Typeable a) => a -> Server a
ping = return

main :: IO ()
main = do
  e <- N.runNowHsT now
  print e
  
