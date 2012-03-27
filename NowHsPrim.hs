module NowHsPrim where

import Interface
import NowHs

import qualified Network.WebSockets as WS

import Data.Aeson
import Control.Monad.Trans
import Control.Monad.Error

runNowHs :: String -> Int -> NowHs a -> IO ()
runNowHs ip port (NowHs n) = WS.runServer ip port $ \rq -> do 
    WS.acceptRequest rq
    liftIO $ putStrLn "Client connected"
    e <- runErrorT n
    either WS.throwWsError (const $ return ()) e

nowHs :: (MonadNowHs m) => Interface m -> m ()
nowHs iface = do
    let liftWS = liftNowHs . lift
        err    = liftNowHs . throwError
    sink <- liftWS $ WS.getSink
    liftNowHs . liftIO $ putStrLn "Sending interface..."
    liftNowHs . liftIO $ WS.sendSink sink . WS.textData $ encode (interfaceExternal iface)
    forever $ do
        msg <- liftWS WS.receiveData
        case decode' msg of
            Nothing -> do
                err . JSONParseError $ "Cannot parse as FunctionCall: " ++ show msg
            Just fcall -> do
                ret <- interfaceInternal iface fcall
                liftNowHs . liftIO $ WS.sendSink sink . WS.textData $ encode ret
