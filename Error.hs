module Error where

import qualified Control.Monad.Error as Err

data Error
    = TooManyArguments
    | MoreArgumentsExpected
    | UnmarshallFail String
      deriving (Show)

instance Err.Error Error