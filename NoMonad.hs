{-# LANGUAGE GeneralizedNewtypeDeriving, PolyKinds #-}
module NoMonad where

import Control.Monad.Identity

newtype NoMonadT m a
    = NoMonadT { runNoMonad :: Identity a
               }
      deriving (Functor, Monad)
