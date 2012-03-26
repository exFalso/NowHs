module TryTH where

import Language.Haskell.TH

lit :: Int -> Q Int
lit i
    | i < 0 = fail "Negative"
    | otherwise = return i
