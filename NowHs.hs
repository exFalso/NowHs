{-# LANGUAGE GeneralizedNewtypeDeriving, DataKinds, GADTs, KindSignatures, FlexibleInstances, ConstraintKinds, StandaloneDeriving, FlexibleContexts, FunctionalDependencies, TypeFamilies, ScopedTypeVariables, UndecidableInstances, DeriveGeneric, OverlappingInstances, RankNTypes, DefaultSignatures, TypeOperators #-}
module NowHs
    ( NowHsT
    , MonadNowHs(..)
    , MonadNowHsError(..)
    , AnyServer(..)
    , AnyClient(..)
    , NowHsEnv(..)
    , embedNoError
    , runNowHsTNoError
    , runNowHsT
    , getClientMap
    , getServerMap
    , Client(..)
    , Server(..)
    , forkServer
    , runServer
    , Argument(..)
    , ToIR(..)
    , Register(..)
    , Call(..)
    ) where

import IDGen
import Error
import Forkable
import Switch
import FunctionID
import Util
import Proxy
import IR
import Phantom

import Prelude hiding (lookup)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Dynamic
import GHC.Generics as G
import qualified Data.IntMap as IM
import qualified Control.Monad.Error as Err
import qualified Data.IntMap as IM
import qualified Data.Aeson as A

newtype NowHsT err s rep iface sm um a
    = NowHsT { unNowHs :: SwitchT err (Err.ErrorT Error)
                          (IfaceT iface
                           (ReaderT (NowHsEnv s rep iface sm)
                            (IDGenT Int um))) a
             }
      deriving (Monad, Functor, MonadIDGen Int, MonadIO)
instance ForkableT (NowHsT False s rep iface sm) where
    forkT f (NowHsT n) = NowHsT $ (forkT . forkT . forkT . forkT $ f) n


instance (SwitchClass err) => MonadTrans (NowHsT err s rep iface sm) where
    lift = NowHsT . lift . lift . lift . lift
nowhsTyCon :: TyCon
nowhsTyCon = mkTyCon3 "Network.NowHs" "Nowhs" "Nowhs"


instance (TypeableBool err, Typeable s, Typeable rep,
          Typeable1 sm, Typeable1 um, Typeable a) => Typeable (NowHsT err s rep iface sm um a) where
    typeOf _
        = mkTyConApp nowhsTyCon
          [ boolRep (Proxy :: Proxy err)
          , typeOf (undefined :: s)
          , typeOf (undefined :: rep)
          , typeOf1 (undefined :: sm x)
          , typeOf1 (undefined :: um x)
          , typeOf (undefined :: a)
          ]

instance (TypeableBool err, Typeable s, Typeable rep,
          Typeable1 sm, Typeable1 um) => Typeable1 (NowHsT err s rep iface sm um) where
    typeOf1 _
        = mkTyConApp nowhsTyCon
          [ boolRep (Proxy :: Proxy err)
          , typeOf (undefined :: s)
          , typeOf (undefined :: rep)
          , typeOf1 (undefined :: sm x)
          , typeOf1 (undefined :: um x)
          ]

type ServerMap s rep iface m = IM.IntMap (AnyServer s rep iface m)

type ClientMap rep m = IM.IntMap (AnyClient rep m)

data NowHsEnv s rep iface m
     = NowHsEnv { clientFuns :: TVar (ClientMap rep m)
                , serverFuns :: TVar (ServerMap s rep iface m)
                }

embedNoError :: (EmbedSwitch err, Monad m) => NowHsT False s rep iface sm m a -> NowHsT err s rep iface sm m a
embedNoError (NowHsT m) = NowHsT (embedSwitchT m)

runNowHsTNoError :: (MonadIO m) => (Binding ClientType -> NowHsT False s rep iface sm m a) -> m a
runNowHsTNoError n = do
  ac <- liftIO $ newTVarIO IM.empty
  as <- liftIO $ newTVarIO IM.empty
  startIDGenT . runReaderT (runIfaceT (\b -> runSwitchOffT $ unNowHs (n b))) $ NowHsEnv ac as

runNowHsT :: (MonadIO m) => NowHsT err s rep iface sm m a -> m (Either Error a)
runNowHsT n = do
  ac <- liftIO $ newTVarIO IM.empty
  as <- liftIO $ newTVarIO IM.empty
  startIDGenT . runReaderT (foldSwitchT Err.runErrorT (liftM Right) $ unNowHs n) $ NowHsEnv ac as



class (Functor m, MonadIO m, MonadIDGen Int m,
       GetRep m ~ rep, Monad (GetUm m)) => MonadNowHs m rep where
    type GetRep m :: *
    type GetErr m :: Bool
    type GetS m :: *
    type GetUm m :: * -> *
    type GetSm m :: * -> *
    type GetIface m :: *
    getEnv :: m (NowHsEnv (GetS m) rep iface (GetSm m))
    liftNowHs :: (GetErr m ~ err, GetS m ~ s,
                  GetSm m ~ sm,
                  GetIface m ~ iface,
                  GetUm m ~ n, GetRep m ~ rep) => NowHsT err s rep iface sm n a -> m a

class MonadNowHsError m where
    nowHsError :: (GetErr m ~ True) => Error -> m a

instance (SwitchClass err, Functor um, MonadIO um) => MonadNowHs (NowHsT err s rep iface sm um) rep where
    type GetRep (NowHsT err s rep iface sm um) = rep
    type GetErr (NowHsT err s rep iface sm um) = err
    type GetS   (NowHsT err s rep iface sm um) = s
    type GetUm  (NowHsT err s rep iface sm um) = um
    type GetSm  (NowHsT err s rep iface sm um) = sm
    getEnv = NowHsT . lift $ ask
    liftNowHs = id

instance (Monad um) => MonadNowHsError (NowHsT True s rep iface sm um) where
    nowHsError = NowHsT . OnT . Err.throwError

getServerMap :: (MonadNowHs m rep) => m (TVar (ServerMap (GetS m) rep (GetIface m) (GetSm m)))
getServerMap = serverFuns <$> getEnv

getClientMap :: (MonadNowHs m rep) => m (TVar (ClientMap rep (GetSm m)))
getClientMap = clientFuns <$> getEnv

data AnyServer s rep iface m where
    AnyServer :: (Typeable f, Call s rep iface m f)  => f -> AnyServer s rep iface m

data AnyClient rep (m :: * -> *) where
    AnyClient :: (Typeable f)  => f -> AnyClient rep m

data Client m a = Client (m [GetRep m]) (FunctionID ClientType)

newtype Server s rep iface m a = Server { unServer :: NowHsT False s rep iface m m a }
    deriving (Monad, Functor, MonadIO)
-- , ForkableT
deriving instance (Monad m, Num i) => MonadIDGen i (Server s rep iface m)

forkServer :: (Monad m, Forkable m n) => Server s rep iface n () -> NowHsT False s rep iface n m ThreadId
forkServer = forkT fork . unServer

instance (MonadIO m, Functor m) => MonadNowHs (Server s rep iface m) rep where
    type GetRep (Server s rep iface m) = rep    
    type GetErr (Server s rep iface m) = False
    type GetS   (Server s rep iface m) = s
    type GetIface (Server s rep iface m) = iface
    type GetUm  (Server s rep iface m) = m
    type GetSm  (Server s rep iface m) = m
    getEnv = Server getEnv
    liftNowHs = Server

runServer :: (MonadIO m) => Server s rep iface m a -> m a
runServer (Server ma) = runNowHsTNoError ma

-- Typeable instance
serverTyCon :: TyCon
serverTyCon = mkTyCon3 "Network.NowHs" "Server" "Server"

instance (Typeable s, Typeable rep, Typeable1 m, Typeable a) => Typeable (Server s rep iface m a) where
    typeOf _
        = mkTyConApp serverTyCon
          [ typeOf (undefined :: s)
          , typeOf (undefined :: rep)
          , typeOf1 (undefined :: m x)
          , typeOf (undefined :: a)
          
          ]
instance (Typeable s, Typeable rep, Typeable1 m) => Typeable1 (Server s rep iface m) where
    typeOf1 _
        = mkTyConApp serverTyCon
          [ typeOf (undefined :: s)
          , typeOf (undefined :: rep)
          , typeOf1 (undefined :: m x)
          ]


-- type family CallCxt s rep (sm :: * -> *) (um :: * -> *) f :: Constraint
-- type instance CallCxt s rep sm um (a -> f) = (FromIR rep (NowHsT True s rep sm um) a, CallCxt s rep sm um f)
-- type instance CallCxt s rep sm um (Server s rep sm a) = ToIR rep (Server s rep sm) a

class (IR rep) => Call s rep iface sm f where
    call :: f -> [Argument rep] -> forall um. (MonadIO um) => NowHsT True s rep iface sm um (Server s rep iface sm (Argument rep))

instance (IR rep, CxtFrom rep a,
          Call s rep iface sm f) => Call s rep iface sm (a -> f) where
    call af (arg : args) = do
      ea <- fromIR arg
      case ea of
        Left err -> nowHsError (UnmarshallFail err)
        Right a -> call (af a) args
    call _ _ = nowHsError MoreArgumentsExpected

instance (IR rep, CxtTo rep a,
          MonadIO sm, Functor sm) => Call s rep iface sm (Server s rep iface sm a) where
    call s [] = return (toIR =<< s)
    call _ _ = nowHsError TooManyArguments



data Argument rep
    = SimpleArgument rep
    | ServerFunction Int
    | ClientFunction Int
      deriving (Show, Read, G.Generic)

instance (A.ToJSON rep) => A.ToJSON (Argument rep)
instance (A.FromJSON rep) => A.FromJSON (Argument rep)

class (IR rep, MonadNowHs m rep) => ToIR rep m a where
    toIR :: a -> m (Argument rep)

instance (IR rep, CxtTo rep a, MonadNowHs m rep) => ToIR rep m a where
    toIR a = return . SimpleArgument . toRep $ a

instance (IR rep, Register fty (b -> f), fty ~ Side f, Call (GetS m) rep (GetIface m) (GetSm m) (b -> f),
          MonadNowHs m rep) => ToIR rep m (b -> f) where
    toIR f = phantomTProxy (Proxy :: Proxy fty) . functionToIR $ f

instance (IR rep, Register ServerType (Server s rep iface n a),
          Call (GetS m) rep (GetIface m) (GetSm m) (Server s rep iface n a),
          MonadNowHs m rep) => ToIR rep m (Server s rep iface n a) where
    toIR s = phantomTProxy (Proxy :: Proxy ServerType) . functionToIR $ s

instance (IR rep, Register ClientType (Client m a), MonadNowHs m rep,
          Call (GetS m) rep (GetIface m) (GetSm m) (Client m a),
          MonadIO m, Functor m) => ToIR rep m (Client m a) where
    toIR c = phantomTProxy (Proxy :: Proxy ClientType) . functionToIR $ c

class FromIR rep m a where
    fromIR :: (MonadIO m) => (Argument rep) -> m (Either String a)

instance (IR rep, CxtFrom rep a, MonadIO m) => FromIR rep m a where
    fromIR (SimpleArgument r) = return . fromRep $ r
    fromIR _ = return $ Left "Non-function value expected"

instance (MonadIO m, Functor m, Typeable (b -> f), Side f ~ fty,
          MonadNowHs m rep, RepToFunction fty (b -> f)) => FromIR rep m (b -> f) where
    fromIR r = repToFunction (Proxy :: Proxy fty) r

instance (MonadIO m, Functor m, MonadNowHs m rep,
          Typeable (Server s rep iface m a)) => FromIR rep m (Server s rep iface m a) where
    fromIR r = repToFunction (Proxy :: Proxy ServerType) r

instance (MonadIO m, Functor m, MonadNowHs m rep,
          Typeable (Client m a)) => FromIR rep m (Client m a) where
    fromIR r = repToFunction (Proxy :: Proxy ClientType) r
    
functionToIR :: forall fty m f rep.
                (Register fty f, MonadIO m, Functor m, MonadNowHs m rep,
                 Call (GetS m) rep (GetIface m) (GetSm m) f) =>
                  f -> PhantomT m fty (Argument rep)
functionToIR f = PhantomT $ do
  fid <- register f :: m (FunctionID fty)
  return $ case fid of
    ClientID i -> ClientFunction i
    ServerID i -> ServerFunction i

class (Typeable f) => RepToFunction (fty :: FunctionType) f where
    repToFunction :: (MonadIO m, Functor m, MonadNowHs m rep) =>
                     Proxy fty -> Argument rep -> m (Either String f)

instance (Typeable f, Side f ~ ServerType) => RepToFunction ServerType f where
    repToFunction _ (ServerFunction i) = lookup (ServerID i)
    repToFunction _ _ = return $ Left "Server function expected"

instance (Typeable f, Side f ~ ClientType) => RepToFunction ClientType f where
    repToFunction _ (ClientFunction i) = lookup (ClientID i)
    repToFunction _ _ = return $ Left "Client function expected"



type family Side f :: FunctionType
type instance Side (Client m a) = ClientType
type instance Side (Server s rep iface m a) = ServerType
type instance Side (a -> f) = Side f

class (fty ~ Side f) => Register fty f where
    register :: (MonadNowHs m rep, Call (GetS m) rep (GetIface m) (GetSm m) f) => f -> m (FunctionID fty)
    lookup :: (MonadNowHs m rep) => FunctionID fty -> m (Either String f)

instance (fty ~ Side f, RegisterFun fty, Typeable f) => Register fty f where
    register = registerFun (Proxy :: Proxy fty)
    lookup = lookupFun (Proxy :: Proxy fty)

class RegisterFun fty where
    registerFun :: (Typeable f, MonadNowHs m rep,
                    Call (GetS m) rep (GetIface m) (GetSm m) f) => Proxy fty -> f -> m (FunctionID fty)
    lookupFun :: (Typeable f, MonadNowHs m rep) => Proxy fty -> FunctionID fty -> m (Either String f)

instance RegisterFun ServerType where
    registerFun _ f = do
      sid <- genFunID
      registerServer sid f
      return sid
    lookupFun _ (ServerID sid) = do
      sm <- getServerMap
      mf <- IM.lookup sid <$> (liftIO . atomically . readTVar $ sm)
      return $ case mf of
                 Nothing -> Left "Server function ID not found"
                 Just (AnyServer g) ->
                     case cast g of
                       Nothing -> Left "Failed to cast functiontype"
                       Just f -> return f

instance RegisterFun ClientType where
    registerFun _ f = do
      cid <- genFunID
      registerClient cid f
      return cid
    lookupFun _ (ClientID sid) = do
      sm <- getClientMap
      mf <- IM.lookup sid <$> (liftIO . atomically . readTVar $ sm)
      return $ case mf of
                 Nothing -> Left "Client function ID not found"
                 Just (AnyClient g) ->
                     case cast g of
                       Nothing -> Left "Failed to cast functiontype"
                       Just f -> return f

registerServer :: (Typeable t, MonadNowHs m rep, Call (GetS m) rep (GetIface m) (GetSm m) t) => FunctionID ServerType -> t -> m ()
registerServer (ServerID sid) f = do
  sm <- getServerMap
  liftIO . atomically . modifyTVar sm . IM.insert sid $ AnyServer f

registerClient :: (Typeable t, MonadNowHs m rep) => FunctionID ClientType -> t -> m ()
registerClient (ClientID cid) f = do
  cm <- getClientMap
  liftIO . atomically . modifyTVar cm . IM.insert cid $ AnyClient f

-- INTERFACE


-- function id -> function name
type Interface = IM.IntMap String

newtype IfaceT iface m a
    = IfaceT { unIfaceT :: ReaderT iface m a }
    deriving (Functor, Monad, MonadIO, MonadTrans, ForkableT)
deriving instance (MonadIDGen i m, Num i) => MonadIDGen i (IfaceT iface m)

newtype Binding fty = Binding [(Int, FunDesc fty)]
    deriving (Show, Monoid)

runIfaceT :: forall m rep iface a. (RegisterInterface m rep iface, MonadNowHs m rep) =>
             (Binding ClientType -> IfaceT iface m a) -> m a
runIfaceT f = do
  (iface, clientBinding) <- runWriterT $ phantomTProxy (Proxy :: Proxy rep) registerInterface
  runReaderT (unIfaceT $ f clientBinding) iface

class (Monad m) => MonadIface iface m | m -> iface where
    getIface :: m iface
instance (Monad m) => MonadIface iface (IfaceT iface m) where
    getIface = IfaceT ask

getsIface :: (MonadIface iface m) => (iface -> f) -> m f
getsIface f = liftM f getIface

infixr 5 :.
data SFuns :: [*] -> * where
    Nil :: SFuns '[]
    (:.) :: (String, f) -> SFuns fs -> SFuns (f ': fs)

registerSFuns :: (RegisterServer m fs) => SFuns fs -> m (Binding ServerType)
registerSFuns fs = liftM snd $ runWriterT (registerServerFuns fs)

class (Monad m) => RegisterServer m fs where
    registerServerFuns :: SFuns fs -> (IdSel ServerType m) ()
instance (Monad m) => RegisterServer m '[] where
    registerServerFuns Nil = return ()
instance (MonadNowHs m rep, GetRep m ~ rep, GetFunctionDesc f,
          Call (GetS m) rep (GetIface m) (GetSm m) f,
          Register ServerType f, RegisterServer m fs) => RegisterServer m (f ': fs) where
    registerServerFuns ((fName, f) :. fs) = do
      let sDesc = phantomProxy (Proxy :: Proxy f) getFunctionDesc
          fDesc = FunDesc fName sDesc :: FunDesc ServerType
      (ServerID sid) <- lift $ register f
      tell $ Binding [(sid, fDesc)]
      registerServerFuns fs

data FunDesc (fty :: FunctionType)
    = FunDesc { funName :: String
              , funDesc :: FunctionDesc
              }
      deriving (Show)

type IdSel fty = WriterT (Binding fty)
-- the map is id -> selector
class (Monad m, Functor m, G.Generic a) => RegisterInterface m rep a where
    registerInterface :: PhantomT (IdSel ClientType m) rep a

instance (Generic a, RegisterIfaceGeneric m rep (Rep a)) => RegisterInterface m rep a where
    registerInterface = to <$> registerIface

class (Monad m, Functor m) => RegisterIfaceGeneric m rep a where
    registerIface :: PhantomT (IdSel ClientType m) rep (a x)

instance (RegisterIfaceGeneric m rep a) => RegisterIfaceGeneric m rep (D1 d (C1 c a)) where
    registerIface = M1 . M1 <$> registerIface
instance (RegisterIfaceGeneric m rep a, RegisterIfaceGeneric m rep b) => RegisterIfaceGeneric m rep (a :*: b) where
    registerIface = liftM2 (:*:) registerIface registerIface
instance (RegisterIfaceFunction m rep a, g ~ (S1 s (K1 p a)), HasSelector g True, Selector s, GetFunctionDesc a) =>
    RegisterIfaceGeneric m rep (S1 s (K1 p a)) where
        registerIface = do
          (ClientID rawFid, f) <- mapPhantomT lift (registerIfaceFun :: PhantomT m rep (FunctionID ClientType, a))
          let functionDesc = phantomProxy (Proxy :: Proxy a) getFunctionDesc
              functionName = selName (undefined :: S1 s (K1 p a) x)
          PhantomT . tell $ Binding [(rawFid, FunDesc functionName functionDesc)]
          return $ M1 (K1 f)

class (Monad m, Functor m) => RegisterIfaceFunction m rep a where
    registerIfaceFun :: PhantomT m rep (FunctionID ClientType, a)

instance (NewClient m rep a) => RegisterIfaceFunction m rep a where
    registerIfaceFun = PhantomT newClient

-- TYPEDESC
data FunctionDesc
    = FunctionDesc FunctionType [TypeDesc] TypeDesc -- type, arguments, return type
      deriving (Show)

data TypeDesc
    = TypeSimple TypeRep
    | TypeFunction FunctionDesc
      deriving (Show)

class GetFunctionDesc f where
    getFunctionDesc :: Phantom f FunctionDesc
instance (GetTypeDesc a, GetFunctionDesc f) => GetFunctionDesc (a -> f) where
    getFunctionDesc = do
      p <- phantom $ (getTypeDesc :: Phantom a TypeDesc)
      FunctionDesc fty ps r <- phantom $ (getFunctionDesc :: Phantom f FunctionDesc)
      return $ FunctionDesc fty (p : ps) r
instance (GetTypeDesc r) => GetFunctionDesc (Client m r) where
    getFunctionDesc = do
      p <- phantom $ (getTypeDesc :: Phantom r TypeDesc)
      return $ FunctionDesc ClientType [] p
instance (GetTypeDesc r) => GetFunctionDesc (Server s rep iface m r) where
    getFunctionDesc = do
      p <- phantom $ (getTypeDesc :: Phantom r TypeDesc)
      return $ FunctionDesc ServerType [] p

class GetTypeDesc f where
    getTypeDesc :: Phantom f TypeDesc
instance (Typeable a) => GetTypeDesc a where
    getTypeDesc = return . TypeSimple $ typeOf (undefined :: a)
instance (GetFunctionDesc (a -> f)) => GetTypeDesc (a -> f) where
    getTypeDesc = TypeFunction <$> getFunctionDesc
instance (GetFunctionDesc (Client m r)) => GetTypeDesc (Client m r) where
    getTypeDesc = TypeFunction <$> getFunctionDesc
instance (GetFunctionDesc (Server s rep iface m r)) => GetTypeDesc (Server s rep iface m r) where
    getTypeDesc = TypeFunction <$> getFunctionDesc

-- NEWCLIENT
newClient :: (NewClient m rep f, Monad m, Functor m) => m (FunctionID ClientType, f)
newClient = do
  (fid, f) <- newClient'
  return (fid, f id)

class (MonadIO m, Functor m, IR rep) => NewClient m rep f | m -> rep where
    newClient' :: m (FunctionID ClientType, (m [Argument rep] -> m [Argument rep]) -> f)

instance (MonadIO m, Functor m, MonadIDGen Int m, IR rep, GetRep m ~ rep, CxtTo rep (Argument rep)) =>
    NewClient m rep (Client m a) where
    newClient' = do
      fid <- genFunID
      return $ (fid, \f -> Client (map toRep <$> f (return [])) fid)

instance (CxtTo rep a, ToIR rep m a, NewClient m rep f, MonadNowHs m rep) =>
    NewClient m rep (a -> f) where
    newClient' = do
      (fid, g) <- newClient'
      return (fid, \f a -> g (liftM2 (:) (toIR a) . f))
