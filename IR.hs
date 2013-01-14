{-# LANGUAGE TypeFamilies, ConstraintKinds, FunctionalDependencies #-}
module IR
    ( IR(..)
    ) where

import GHC.Prim
import Control.Monad.IO.Class

class IR m rep | m -> rep where
    type Context rep m a :: Constraint
    toIR :: (Context rep m a, MonadIO m) => a -> m rep
    fromIR :: (Context rep m a, MonadIO m) => rep -> m (Either String a)

