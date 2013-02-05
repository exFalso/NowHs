{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, ScopedTypeVariables #-}
module Try where

import IR
import Argument
import qualified NowHs as N
import qualified Server as S
import qualified Data.Aeson as A

import Prelude hiding (lookup)
import Data.Typeable


type NowHs = N.NowHsT False A.Value IO
type Server = S.Server () A.Value IO

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
  
