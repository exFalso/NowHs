{-# LANGUAGE TemplateHaskell #-}
module Main where

import NowHs
import NowHsPrim
import FunctionCall
import Interface
import SchemaTH

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Applicative
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.TH
import Data.Set

data SomeData = SomeData { first :: Int, second :: Int }
$(deriveJSON id ''SomeData)
$(deriveSchema ''SomeData)

asd :: Int -> Int -> NowHs Integer
asd i1 i2 = liftIO $ do
    let i3 = fromIntegral $ i1 + i2
    putStrLn $ show i1 ++ " + " ++ show i2 ++ " = " ++ show i3
    return i3

afg :: SomeData -> NowHs Int
afg d = return $ first d

interface :: Interface NowHs
interface = $(genInterface ['asd, 'afg])

main = nowHsServer "127.0.0.1" 8888 interface id


