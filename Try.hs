{-# LANGUAGE TemplateHaskell, FlexibleInstances, FlexibleContexts, GeneralizedNewtypeDeriving #-}
module Main where

import NowHs
import NowHsPrim
import Interface
import SchemaTH

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.State as S
import Data.Aeson
import Data.Aeson.TH
import Data.Set

data SomeData = SomeData { first :: Int, second :: Int }
$(deriveJSON id ''SomeData)
$(deriveSchema ''SomeData)

newtype MyM a = MyM (StateT Int NowHs a)
            deriving (Monad, MonadNowHs, MonadIO, MonadState Int)

runMyM (MyM m) = runStateT m

asd :: Int -> Int -> (Int -> Client Integer) -> m ()
asd i1 i2 ret = do
    modify (* 2)
    liftIO $ do
        let i3 = fromIntegral $ i1 + i2
        putStrLn $ show i1 ++ " + " ++ show i2 ++ " = " ++ show i3
        return i3
    ret i3

afg :: SomeData -> MyM Int
afg d = do
    modify (+ 1)
    return $ first d

prnt :: MyM Int
prnt = do
    i <- get
    liftIO $ print i
    return i

interface :: Interface MyM
interface = $(genInterface [ 'asd
                           , 'afg 
                           , 'prnt ])

main = runNowHs "127.0.0.1" 8888 . flip runMyM 0 . nowHs $ interface


