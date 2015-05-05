module Tools.Gen where

import Control.Monad.State

type Gen a r = State a r

fresh :: Enum a => Gen a a
fresh = do
    x <- get
    put (succ x)
    return x

