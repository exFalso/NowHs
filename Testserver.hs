{-# LANGUAGE TemplateHaskell #-}

module Main where

import NowHs
import NowHsPrim
import Interface
import SchemaTH
import Data.Aeson.TH


sum_rpc :: Int -> Int -> NowHs () Int
sum_rpc a b = return (a + b)


data User = User { name :: String, age :: Int }
$(deriveJSON id ''User)
$(deriveSchema ''User)

-- TODO needs HasSchema Bool
--authenticated_rpc :: User -> NowHs () Bool
--authenticated_rpc user = return $ name user >= 18

firstUser_rpc :: NowHs () User
firstUser_rpc = return $ User { name = "John", age = 17 }


interface :: Interface ()
interface = $(genInterface [ 'sum_rpc
                           --, 'authenticated_rpc
                           , 'firstUser_rpc
                           ])

main = runNowHs "127.0.0.1" 8888 () . nowHs $ interface
