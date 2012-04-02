module NowHsPrim where

import Interface
import NowHs

import qualified Network.WebSockets as WS

import Data.Aeson
import Control.Async
import Control.Monad.Trans
import Control.Monad.Error

runNowHs :: String -> Int -> NowHs a -> IO ()
runNowHs ip port (NowHs n) = WS.runServer ip port $ \rq -> do 
    WS.acceptRequest rq
    liftIO $ putStrLn "Client connected"
    e <- wait $ runErrorT n
    either WS.throwWsError (const $ return ()) e

nowHs :: (MonadNowHs m) => Interface m -> m ()
nowHs iface = do
    let liftWS = liftNowHs . lift . lift
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


nowHsRun :: (MonadNowHs m) => Interface -> NowHs ()
nowHsRun frk _ = do
    ...
        msg <- liftWS WS.receiveData
         $ processMsg msg


newtype NowHsT s m a = NowHs (ReaderT (TVar s) m a)

type NowHsOur s = NowHsT s WS.WebSockets

type NowHs s = NowHsT s IO

instance MonadState s NowHs where
    put aasd = do
        tv <- NowHs ask
        liftIO . atomically $ writeTVar tv aasd
    get = do
        tv <- NowHs ask
        liftIO . atomically $ readTVar tv

nowHs :: (MonadNowHs m, Forkable m) => Interface m -> m ()
nowHs = nowHsRun fork