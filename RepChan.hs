{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, GeneralizedNewtypeDeriving, DefaultSignatures, FlexibleInstances, FlexibleContexts #-}
module RepChan where

import Error
import Forkable

import Control.Monad.Reader
import Control.Applicative
import System.IO
import Data.Aeson
import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.Attoparsec

import qualified Control.Monad.Error as Err
import qualified Data.Conduit.List as CList
import qualified Network.WebSockets as WS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Attoparsec as Atto

class Serialisable a where
    serialise :: a -> BS.ByteString

class (Monad m) => MonadPutBS m where
    putBS :: Sink BS.ByteString m ()

class (Monad m) => MonadGetBS m where
    getBS :: Source m BS.ByteString

class Parseable a where
    parse :: Atto.Parser a

-- no receive !! should only be done by master thread
class (Monad m) => MonadSend rep m where
    send :: Sink rep m ()

    default send :: (Serialisable rep, MonadPutBS m) => Sink rep m ()
    send = CList.map serialise =$ putBS

class (Monad m, Err.MonadError Error m) => MonadReceive rep m where
    receive :: Source m rep

    default receive :: (Parseable rep, MonadGetBS m, MonadThrow m) => Source m rep
    receive = getBS $= conduitParser parse =$= CList.map snd

newtype HandleM a
    = HandleM (ReaderT Handle IO a)
      deriving (Monad, Functor, Applicative, MonadIO, Forkable)

getHandle :: HandleM Handle
getHandle = HandleM ask

withHandle :: (Handle -> HandleM a) -> HandleM a
withHandle f = f =<< getHandle

runHandleM :: Handle -> HandleM a -> IO a
runHandleM h (HandleM m) = runReaderT m h

newtype SinkM p a
    = SinkM (ReaderT (WS.Sink p) IO a)
      deriving (Monad, Functor, Applicative, MonadIO, Forkable)

getSink :: SinkM p (WS.Sink p)
getSink = SinkM ask

runSinkM :: WS.Sink p -> SinkM p a -> IO a
runSinkM h (SinkM m) = runReaderT m h

instance Serialisable Value where
    serialise = BSL.toStrict . encode

instance MonadPutBS HandleM where
    putBS = lift getHandle >>= sinkHandle

instance MonadPutBS (SinkM p) where
    putBS = lift getSink >>= \s -> CList.mapM_ (liftIO . WS.sendSink s . WS.DataMessage . WS.Binary . BSL.fromStrict)


