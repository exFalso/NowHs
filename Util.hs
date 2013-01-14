{-# LANGUAGE KindSignatures, DataKinds, TypeOperators, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
module Util where

import GHC.Generics

class HasSelector (f :: * -> *) (b :: Bool) | f -> b
instance HasSelector (M1 S NoSelector t) False
instance (HasSelector f b) => HasSelector (M1 S s f) b
instance HasSelector (K1 p a) True
instance HasSelector U1 False
instance (HasSelector f1 b) => HasSelector (f1 :*: f2) b
