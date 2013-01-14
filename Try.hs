{-# LANGUAGE FlexibleInstances, TypeFamilies #-}
module Try where

import IR

instance IR String where
    type Context String a = (Show a, Read a)
    toIR = show
    fromIR s = case reads s of
                 ((a, _) : _) -> return a
                 [] -> Left "Failed to Read value"
