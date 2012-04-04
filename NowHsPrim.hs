{-# LANGUAGE NamedFieldPuns #-}

module NowHsPrim where

import Interface
import Message
import NowHs

import qualified Network.WebSockets as WS

import Data.Aeson
import Control.Concurrent.STM
import Control.Monad.Trans
import Control.Monad.Error
import Control.Monad.Reader

runNowHs :: String -> Int -> s -> NowHsWs s () -> IO ()
runNowHs ip port sessionState (NowHs n) = WS.runServer ip port $ \rq -> do
    WS.acceptRequest rq
    liftIO $ putStrLn "Client connected"
    sessionStateTVar <- liftIO $ newTVarIO sessionState
    e <- runErrorT $ runReaderT n sessionStateTVar
    either WS.throwWsError (const $ return ()) e


nowHs :: Interface s -> NowHsWs s ()
nowHs iface = do
    sink <- lift WS.getSink
    liftIO $ putStrLn "Sending interface..."
    liftIO $ WS.sendSink sink . WS.textData $ encode (interfaceExternal iface)
    forever $ do
        msg <- lift WS.receiveData
        case decode' msg of
            Nothing -> do
                throwError . JSONParseError $ "Cannot parse as ClientMessage: " ++ show msg
            Just x -> case x of
                ClientFCall { cFunName, cFunId, cFunArgs } -> void . forkNowHs $ do
                    ret <- interfaceInternal iface (cFunName, cFunArgs)
                    liftIO $ WS.sendSink sink . WS.textData $
                        encode ServerFunctionReturn { sFunId = cFunId , sRetVal = ret }
