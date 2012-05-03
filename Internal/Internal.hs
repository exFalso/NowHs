{-# LANGUAGE GeneralizedNewtypeDeriving, ExistentialQuantification, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances, DataKinds, TypeFamilies, ScopedTypeVariables, StandaloneDeriving, OverlappingInstances, DeriveDataTypeable, GADTs, DeriveGeneric, Rank2Types, NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Internal.Internal ( InternalT(..)
                         , InternalState(..)
                         , Server
                         , ServerFunCall(..)
                         , ServerCall (..)
                         , Client) where

import Internal.Messages
import JSON
import SharedState

import GHC.Generics
import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Concurrent.STM
import qualified Data.IntMap as IMap
import qualified Data.Map as Map
import qualified Data.Aeson as Aeson
import qualified Network.WebSockets as WS
import Data.Dynamic
import Data.Maybe

data NowHsError
    = TypeMismatch String
    | MoreArgumentsExpected
    | TooManyArguments
    | InvalidFunctionId
    | JSONError String
    | InvalidClientFunctionName String
      deriving (Show)
instance Error NowHsError

newtype Server s a = Server { unServer :: InternalIO s a }
    deriving (Functor, Monad, Typeable, MonadIO)

-- phantom type only tells how to decode json
data Client s a
    = Client { clientArgs :: AnyInternal s [Aeson.Value]
             , clientCid :: ClientId }
      deriving (Typeable)

data ServerFunCall s = forall a b. (ServerCall a s b, ServerCallDyn a s) => ServerFunCall a
type ClientFunCall s = [Aeson.Value] -> AnyInternal s Aeson.Value

data AnyInternal s a = AnyInternal
    { applyAnyInternal :: forall m. (Functor m, Monad m) => InternalT s m a }

instance Functor (AnyInternal s) where
    fmap f (AnyInternal a) = AnyInternal (fmap f a)
instance Applicative (AnyInternal s) where
    pure a = AnyInternal (return a)
    (AnyInternal af) <*> (AnyInternal aa) = AnyInternal (af <*> aa)
instance Monad (AnyInternal s) where
    return = pure
    (AnyInternal ma) >>= f = AnyInternal (ma >>= applyAnyInternal . f)



data InternalState s
    = InternalState
      -- nonpersistent server functions
      { internalStateServerCallMap :: IMap.IntMap (ServerFunCall s)
      -- nonpersistent client functions
      , internalStateClientCallMap :: IMap.IntMap (ClientFunCall s)
      -- for generating unique client ids
      , nextSid :: ServerId
      }

data InternalEnv s
    = InternalEnv
      -- persistent server functions
      { internalEnvServerCallMapPers :: IMap.IntMap (ServerFunCall s)
      -- persistent client functions
      , internalEnvClientCallMapPers :: IMap.IntMap (ClientFunCall s)
      , internalEnvClientNameIdMap :: Map.Map String Int
      , internalEnvSink :: WS.Sink WS.Hybi10
      , internalEnvUserState :: TVar s
      }


newtype InternalT s m a = InternalT { unInternal :: ErrorT NowHsError
                                                    (StateT (InternalState s)
                                                     (ReaderT (InternalEnv s) m)) a }
    deriving ( Functor, Monad, Applicative, MonadIO
             , MonadReader (InternalEnv s)
             , MonadState (InternalState s)
             , MonadError NowHsError)

type InternalIO s = InternalT s IO
liftInternalIO :: (MonadIO m) => InternalT s IO a -> InternalT s m a
liftInternalIO = InternalT . mapErrorT (mapStateT (mapReaderT liftIO)) . unInternal

runInternalT :: (Monad m) => InternalState s -> InternalEnv s -> InternalT s m a ->
                m (Either NowHsError a)
runInternalT s r i = runReaderT (evalStateT (runErrorT (unInternal i)) s) r

class (NowHsType s r) => ServerCall a s r | a -> r, a -> s where
    callServer :: a -> [Aeson.Value] -> InternalIO s r
instance (ServerCall f s r, NowHsType s a) => ServerCall (a -> f) s r where
    callServer f (v : vs) = do
      a <- nowHsFromJSON v
      callServer (f a) vs
    callServer _ [] = throwError MoreArgumentsExpected

instance (NowHsType s a) => ServerCall (Server s a) s a where
    callServer s [] = unServer s
    callServer _ _ = throwError TooManyArguments

data FunctionType = ServerType | ClientType

jsonInternal :: (JSON a, Monad m) => Aeson.Value -> InternalT s m a
jsonInternal v = case fromJSON v of
                   Aeson.Error e -> throwError (JSONError e)
                   Aeson.Success a -> return a

registerFunction :: (ServerCall a s b, ServerCallDyn a s, Monad m) =>
                    a -> InternalT s m ServerId
registerFunction f = do
  s@InternalState { internalStateServerCallMap = scMap
                  , nextSid = ServerId sid } <- get
  put s { internalStateServerCallMap = IMap.insert sid (ServerFunCall f) scMap
        , nextSid = ServerId (sid + 1)}
  return (ServerId sid)
  

-- this is to create a type that has a JSON underlying representation
class NowHsType s a where
    nowHsFromJSON :: Aeson.Value -> InternalIO s a
    nowHsToJSON :: (Monad m, Functor m) => a -> InternalT s m Aeson.Value
-- we need all these so that it doesnt overlap too much
instance ( fty ~ GetFunctionType (a -> b)
         , NowHsTypeFun s (a -> b) fty) => NowHsType s (a -> b) where
    nowHsFromJSON v = createInstanceFun (Proxy :: Proxy fty) v 
    nowHsToJSON a = getFunctionId (Proxy :: Proxy fty) a
instance ( NowHsType s a, Typeable a
         , NowHsTypeFun s (Server s a) ServerType) => NowHsType s (Server s a) where
    nowHsFromJSON v = createInstanceFun (Proxy :: Proxy ServerType) v 
    nowHsToJSON s = toJSON <$> registerFunction s
instance (NowHsTypeFun s (Client s a) ClientType) => NowHsType s (Client s a) where
    nowHsFromJSON v = createInstanceFun (Proxy :: Proxy ClientType) v 
    nowHsToJSON Client { clientCid = cid } = return (toJSON cid)
instance (JSON a) => NowHsType s a where
    nowHsFromJSON v
        = case fromJSON v of
            Aeson.Error e -> throwError . JSONError $ e
            Aeson.Success a -> return a
    nowHsToJSON = return . toJSON
-- NO PERSISTENT MAP FOR CLIENT FUNCTIONS
type family GetFunctionType a :: FunctionType
type instance GetFunctionType (Server s a) = ServerType
type instance GetFunctionType (Client s a) = ClientType
type instance GetFunctionType (a -> r) = GetFunctionType r

class NowHsTypeFun s a (fty :: FunctionType) where
    createInstanceFun :: Proxy fty -> Aeson.Value -> InternalIO s a
    getFunctionId :: (Monad m, Functor m) => Proxy fty -> a -> InternalT s m Aeson.Value

-- server side function
instance (ServerCall f s r, ServerCallDyn f s, ServerFun f s r) =>
    NowHsTypeFun s f ServerType where
    createInstanceFun _ v = do
      sid <- jsonInternal v
      ($ []) <$> createServerFun sid
    getFunctionId _ f = toJSON <$> registerFunction f

instance (ClientFun f s r) => NowHsTypeFun s f ClientType where
    createInstanceFun _ v = do
      cid <- jsonInternal v
      ($ return []) <$> createClientFun cid
    getFunctionId _ f = toJSON <$> getClientFunctionId f

-- to create an instance of a server function to expose to the user
-- we dont actually need the return type, oh well
class ServerFun f s (r :: *) | f -> r, f -> s where
    createServerFun :: (Monad m) => ServerId -> InternalT s m ([Dynamic] -> f)

instance (Typeable a, ServerFun f s r) => ServerFun (a -> f) s r where
    createServerFun sid = do
      f <- createServerFun sid
      return (\dys a -> f (toDyn a : dys))

instance (Typeable a) => ServerFun (Server s a) s a where
    createServerFun sid = do
      -- lookup ServerCallDyn (existential) function in Server's map (sid->function) and apply
      mp <- gets internalStateServerCallMap
      exF <- maybe (throwError InvalidFunctionId) return $ IMap.lookup (serverId sid) mp
      case exF of               -- existential, no bind
        ServerFunCall f ->
            return $ \dys ->
                do
                  res <- callServerDyn f dys
                  case fromDynamic res of
                    Nothing -> Server . throwError . TypeMismatch . show $
                               typeOf (undefined :: a)
                    Just a -> return a

-- to create an instance of a client function to expose to the user
class ClientFun f s (r :: *) | f -> r, f -> s where
    createClientFun :: (Monad m, Functor m) =>
                       ClientId -> InternalT s m (AnyInternal s [Aeson.Value] -> f)
    getClientFunctionId :: (Monad m) => f -> InternalT s m ClientId

instance (ClientFun f s r, NowHsType s a) => ClientFun (a -> f) s r where
    createClientFun cid = do
      f <- createClientFun cid
      return (\dys a -> f (liftA2 (:) (AnyInternal (nowHsToJSON a)) dys))
    getClientFunctionId f = getClientFunctionId (f undefined) -- /whistle

instance ClientFun (Client s a) s a where
    createClientFun cid = return (\args -> Client args cid)
    getClientFunctionId Client { clientCid = cid } = return cid

-- to call a server function with Dynamic arguments
class ServerCallDyn f s | f -> s where
    callServerDyn :: f -> [Dynamic] -> Server s Dynamic

instance (ServerCallDyn f s, Typeable a) => ServerCallDyn (a -> f) s where
    callServerDyn f (dy : dys) =
        case fromDynamic dy of
            Nothing -> Server . throwError . TypeMismatch . show $
                       typeOf (undefined :: a)
            Just a -> callServerDyn (f a) dys
    callServerDyn _ [] = Server $ throwError MoreArgumentsExpected

instance (Typeable a) => ServerCallDyn (Server s a) s where
    callServerDyn f [] = toDyn <$> f
    callServerDyn _ _ = Server $ throwError TooManyArguments


echo :: (Int -> Server s Int) -> Client s Int
echo = clientFunction "echo"


class ClientFunction f s | f -> s where
    clientFunction :: String -> f
    clientFunction = clientFunction' (return [])
    clientFunction' :: AnyInternal s [Aeson.Value] -> String -> f
instance (NowHsType s a) => ClientFunction (Client s a) s where
    clientFunction' args str = Client args (ClientFunNameId str)
instance (NowHsType s a, ClientFunction f s) => ClientFunction (a -> f) s where
    clientFunction' args str a = clientFunction'
                                 (liftA2 (:) (AnyInternal (nowHsToJSON a)) args) str

asyncClient :: Client s a -> Server s ()
asyncClient Client { clientArgs, clientCid } = Server $ do
    args <- applyAnyInternal clientArgs
    rCid <- rawClientId clientCid
    sendMessage (AsyncCall rCid args)

rawClientId :: (Monad m) => ClientId -> InternalT s m RawClientId
rawClientId (ClientId i) = return i
rawClientId (ClientFunNameId nam) = do
    mp <- asks internalEnvClientNameIdMap
    case Map.lookup nam mp of
        Nothing -> throwError $ InvalidClientFunctionName nam
        Just a -> return a

syncClient :: (NowHsType s a, Typeable a) => Client s a -> Server s a
syncClient Client { clientArgs, clientCid } = Server $ do
    args <- applyAnyInternal clientArgs
    rCid <- rawClientId clientCid
    tvar <- liftIO $ newTVarIO Nothing
    ServerId rSid <- registerFunction (syncClientCallback tvar)
    sendMessage (SyncCall rCid args rSid)
    liftIO . atomically $ liftA2 fromMaybe retry (readTVar tvar)

syncClientCallback :: (NowHsType s a) => TVar (Maybe a) -> a -> Server s ()
syncClientCallback tvar a = liftIO . atomically $ writeTVar tvar (Just a)

sendMessage :: (MonadIO m) => ServerMessage -> InternalT s m ()
sendMessage msg = do
    sink <- asks internalEnvSink
    liftIO . WS.sendSink sink . WS.textData . Aeson.encode $ toJSON msg


