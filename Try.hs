{-# LANGUAGE TemplateHaskell #-}
module Main where

import NowHs
import NowHsPrim
import Interface
import SchemaTH

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Applicative
import Control.Concurrent
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.TH
import Data.Set

data SomeData = SomeData { first :: Int, second :: Int }
$(deriveJSON id ''SomeData)
$(deriveSchema ''SomeData)

-- TODO write test that checks for asynchronousness superforking!

asd :: Int -> Int -> NowHs () Integer
asd i1 i2 = liftIO $ do
    let i3 = fromIntegral $ i1 + i2
    putStrLn $ show i1 ++ " + " ++ show i2 ++ " = " ++ show i3
    let seconds = 1000000
    liftIO $ threadDelay (5 * seconds)
    return i3

afg :: SomeData -> NowHs () Int
afg d = return $ first d

interface :: Interface ()
interface = $(genInterface [ 'asd
                           , 'afg])

main = runNowHs "127.0.0.1" 8888 () . nowHs $ interface


