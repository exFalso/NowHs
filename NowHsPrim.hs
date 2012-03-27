module NowHsPrim where

import Interface
import NowHs

import qualified Network.WebSockets as WS

import Data.Aeson
import Control.Monad.Trans
import Control.Monad.Error

nowHsServer :: (MonadNowHs m) => String -> Int -> Interface m -> (m () -> NowHs ()) -> IO ()
nowHsServer ip port iface run = WS.runServer ip port $ \rq -> do
    WS.acceptRequest rq
    liftIO $ putStrLn "Client connected"
    nowHsWS iface run

nowHsWS :: (MonadNowHs m) => Interface m -> (m () -> NowHs ()) -> WS.WebSockets Prot ()
nowHsWS iface run = do
    sink <- WS.getSink
    e <- runNowHs . run $ nowHsM iface sink
    either WS.throwWsError return e

nowHsM :: (MonadNowHs m) => Interface m -> WS.Sink Prot -> m ()
nowHsM iface sink = do
    let liftWS = liftNowHs . lift
        err    = liftNowHs . throwError
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
