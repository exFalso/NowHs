{-# LANGUAGE DefaultSignatures, FlexibleContexts, FlexibleInstances, StandaloneDeriving, DeriveGeneric, TypeOperators, FunctionalDependencies, MultiParamTypeClasses, UndecidableInstances, ScopedTypeVariables, OverloadedStrings, OverlappingInstances, KindSignatures, EmptyDataDecls, DataKinds, PolyKinds #-}

module JSON where

import GHC.Generics
import Control.Monad.State
import Control.Applicative
import Control.Arrow (second)
import Data.List

import qualified Data.Aeson as Aeson
import qualified Data.Vector as Vector
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Text as Text

class JSON a where
    toJSON :: a -> Aeson.Value
    fromJSON :: Aeson.Value -> Aeson.Result a
    
    default toJSON :: (Generic a, GJSON (Rep a)) => a -> Aeson.Value
    toJSON = gtoJSON . from
    default fromJSON :: (Generic a, GJSON (Rep a)) => Aeson.Value -> Aeson.Result a
    fromJSON = fmap to . gfromJSON

class GJSON (f :: * -> *) where    
    gtoJSON :: f x -> Aeson.Value
    gfromJSON :: Aeson.Value -> Aeson.Result (f x)

data Tree a = Leaf (a) | Branch {leftBranch :: (Tree a), rightBranch :: (Tree a) }
              deriving (Show)
deriving instance Generic (Tree a)
instance (JSON a) => JSON (Tree a)

data Wat = Wat1 { wat :: Int , haha :: String}
           deriving (Show)
deriving instance Generic Wat

data Wat2 = Wat21 Int String
           deriving (Show)
deriving instance Generic Wat2
instance JSON Wat

instance JSON () where
    toJSON = Aeson.toJSON
    fromJSON = Aeson.fromJSON

instance JSON Int where
    toJSON = Aeson.toJSON
    fromJSON = Aeson.fromJSON

instance JSON Char where
    toJSON = Aeson.toJSON
    fromJSON = Aeson.fromJSON

instance JSON String where
    toJSON = Aeson.toJSON
    fromJSON = Aeson.fromJSON

instance JSON a => JSON [a] where
    toJSON = Aeson.Array . Vector.fromList . map toJSON
    fromJSON (Aeson.Array v) = mapM fromJSON . Vector.toList $ v
    fromJSON _ = Aeson.Error "Array expected"

instance JSON Aeson.Value where
    toJSON = id
    fromJSON = return

instance (JSON a, JSON b) => JSON (a, b)

-- aasd :: Aeson.Result Wat
-- aasd = fromJSON . toJSON $ Wat1 1 "asd"

-- fdgh :: Aeson.Result (Tree (Int, Int))
-- fdgh = fromJSON . toJSON $ Branch (Leaf (1 :: Int, 2 :: Int)) (Leaf (3, 4))

-- data Iden a = Iden a
--               deriving (Show)
-- deriving instance Generic (Iden a)
-- instance (JSON a) => JSON (Iden a)

-- -- fdgh2 :: Aeson.Result (Tree Int)
-- fdgh2 :: Aeson.Value
-- fdgh2 = toJSON $ Iden (1 :: Int, 2 :: Int)

-- main :: IO ()
-- main = test2

-- test :: IO ()
-- test = print $ (fromJSON fdgh2 :: Aeson.Result (Iden (Int, Int)))

-- test2 :: IO ()
-- test2 = print $ fdgh

-- instance Datatype w => Show (M1 D w t x) where
--     show a = datatypeName a

data Proxy1 (k :: * -> *) = Proxy1
data Proxy k = Proxy


_SELECTOR_NAME :: Text.Text
_SELECTOR_NAME = "constr"

instance (GJSONSum (f1 :+: f2)) => GJSON (M1 D dn (f1 :+: f2)) where
    gtoJSON (M1 a) = gtoJSONSum a
    gfromJSON (Aeson.Object hmap) = do
        let fromCnstr = HashMap.fromList $ gfromJSONSum
        (Aeson.String sel) <- hmapLookup _SELECTOR_NAME
        constr <- case HashMap.lookup sel fromCnstr of
                  Nothing -> Aeson.Error $ "Invalid constructor \"" ++ Text.unpack sel ++ "\""
                  Just constr -> return constr
        val <- hmapLookup sel
        M1 <$> constr val
        where
          hmapLookup field
              = case HashMap.lookup field hmap of
                  Nothing -> Aeson.Error $ "\"" ++ Text.unpack _SELECTOR_NAME ++
                             "\" field missing"
                  Just sel -> return sel
    gfromJSON _ = Aeson.Error "Object expected"

class GJSONSum f where
    gtoJSONSum :: f x -> Aeson.Value
    gfromJSONSum :: [(Text.Text, Aeson.Value -> Aeson.Result (f x))]

instance (GJSONSum f1, GJSONSum f2) => GJSONSum (f1 :+: f2) where
    gtoJSONSum (L1 l) = gtoJSONSum l
    gtoJSONSum (R1 r) = gtoJSONSum r
    gfromJSONSum = map (second (fmap L1 .)) leftMap ++
                     map (second (fmap R1 .)) rightMap
        where
          leftMap = gfromJSONSum
          rightMap = gfromJSONSum

instance (GJSONProd f, Constructor cn) => GJSONSum (M1 C cn f) where
    gtoJSONSum (M1 a)
        = let cName = Text.pack $ conName (undefined :: (M1 C cn f x)) in
          Aeson.object [ _SELECTOR_NAME Aeson..= Aeson.String cName
                       , cName Aeson..= gtoJSONProd a ]
    gfromJSONSum
        = let cName = Text.pack $ conName (undefined :: (M1 C cn f x)) in
          [(cName, fmap M1 . gfromJSONProd)]

instance (GJSONProd f) => GJSON (M1 D dn (M1 C cn f)) where
    gtoJSON (M1 (M1 a)) = gtoJSONProd a
    gfromJSON a = M1 . M1 <$> gfromJSONProd a

class GJSONProd f where
    gtoJSONProd :: f x -> Aeson.Value
    gfromJSONProd :: Aeson.Value -> Aeson.Result (f x)

-- explicit traversal just to know whether it's a record or not
class HasSelector (f :: * -> *) (b :: Bool) | f -> b

instance HasSelector (M1 S NoSelector t) False
instance (HasSelector f b) => HasSelector (M1 S s f) b
instance HasSelector (K1 p a) True
instance HasSelector U1 False
instance (HasSelector f1 b) => HasSelector (f1 :*: f2) b

class GJSONProdSel f (b :: Bool) where
    gtoJSONProdSel   :: Proxy b -> f x -> Aeson.Value
    gfromJSONProdSel :: Proxy b -> Aeson.Value -> Aeson.Result (f x)

instance (HasSelector f b, GJSONProdSel f b) => GJSONProd f where
    gtoJSONProd   = gtoJSONProdSel (Proxy :: Proxy b)
    gfromJSONProd = gfromJSONProdSel (Proxy :: Proxy b)

-- no selectors
instance (GJSONList (f1 :*: f2)) => GJSONProdSel (f1 :*: f2) False where
    gtoJSONProdSel _ f                   = Aeson.Array . Vector.fromList . gtoJSONList f $ []
    gfromJSONProdSel _ (Aeson.Array vec) = do
      (r, []) <- runStateT gfromJSONList $ Vector.toList vec
      return r
    gfromJSONProdSel _ _                 = Aeson.Error "Array expected"

instance (JSON a) => GJSONProdSel (M1 S NoSelector (K1 p a)) False where
    gtoJSONProdSel _ (M1 (K1 a)) = toJSON a
    gfromJSONProdSel _ v         = M1 . K1 <$> fromJSON v

instance GJSONProdSel U1 False where
    gtoJSONProdSel _ _ = Aeson.Array Vector.empty
    gfromJSONProdSel _ (Aeson.Array v)
        | v == Vector.empty = return U1
    gfromJSONProdSel _ _ = Aeson.Error "Empty array expected"

class GJSONList f where
    gtoJSONList   :: f x -> [Aeson.Value] -> [Aeson.Value]
    gfromJSONList :: StateT [Aeson.Value] Aeson.Result (f x)

instance (GJSONList f1, GJSONList f2) => GJSONList (f1 :*: f2) where
    gtoJSONList (l :*: r) = gtoJSONList l . gtoJSONList r
    gfromJSONList         = liftM2 (:*:) gfromJSONList gfromJSONList

instance (JSON a) => GJSONList (M1 S NoSelector (K1 p a)) where
    gtoJSONList (M1 (K1 a)) = (toJSON a :)
    gfromJSONList           = do
      (v : rest) <- get
      put rest
      M1 . K1 <$> lift (fromJSON v)

-- selectors


instance (HasSelOrder f, GJSONAssoc f) => GJSONProdSel f True where
    gtoJSONProdSel _ f = Aeson.Object . HashMap.fromList . gtoJSONAssoc f $ []
    gfromJSONProdSel _ (Aeson.Object hmap) = do
      -- we need to order hmapHashMap.toList hmap
      lst <- mapM match $ HashMap.toList hmap
      let ordLst = sortBy (\(a, _) (b, _) -> compare a b) lst
      (r, []) <- runStateT gfromJSONAssoc $ map snd ordLst
      return r
          where
            ordr = order (Proxy1 :: Proxy1 f)
            match (s, v) = case HashMap.lookup s ordr of
                Nothing -> Aeson.Error $ "\"" ++ show s ++ "\" is not a valid field name"
                Just i -> return (i, v)
    gfromJSONProdSel _ _                 = Aeson.Error "Object expected"

type SelOrder = State (Int, HashMap.HashMap Text.Text Int)

order :: (HasSelOrder f) => Proxy1 f -> HashMap.HashMap Text.Text Int
order p = snd . snd $ runState (getSelOrder p) (0, HashMap.empty)

class HasSelOrder (f :: * -> *) where
    getSelOrder :: Proxy1 f -> SelOrder ()

instance (HasSelOrder f1, HasSelOrder f2) => HasSelOrder (f1 :*: f2) where
    getSelOrder _ = getSelOrder (Proxy1 :: Proxy1 f1) >> getSelOrder (Proxy1 :: Proxy1 f2)

instance (Selector s) => HasSelOrder (M1 S s (K1 p a)) where
    getSelOrder _ = do
      (i, m) <- get
      put (i + 1, HashMap.insert (Text.pack $ selName (undefined :: (M1 S s (K1 p a) x))) i m)

class GJSONAssoc f where
    gtoJSONAssoc :: f x -> [(Text.Text, Aeson.Value)] -> [(Text.Text, Aeson.Value)]
    gfromJSONAssoc :: StateT [Aeson.Value] Aeson.Result (f x)

instance (JSON a, Selector s) => GJSONAssoc (M1 S s (K1 p a)) where
    gtoJSONAssoc (M1 (K1 a)) = let sn = selName (undefined :: (M1 S s (K1 p a)) x) in
                               ((Text.pack sn, toJSON a) :)
    gfromJSONAssoc = do
      (v : rest) <- get
      put rest
      M1 . K1 <$> lift (fromJSON v)

instance (GJSONAssoc f1, GJSONAssoc f2) => GJSONAssoc (f1 :*: f2) where
    gtoJSONAssoc (l :*: r) = gtoJSONAssoc l . gtoJSONAssoc r
    gfromJSONAssoc         = liftM2 (:*:) gfromJSONAssoc gfromJSONAssoc
