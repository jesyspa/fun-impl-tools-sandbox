module AST.Simple where

import Bound
import Control.Monad (ap)
import Prelude.Extras

data Op = Plus | Minus | Times | Divide
        deriving (Eq, Ord, Read, Show)

data Exp a = Var a
           | NumLit Int
           | Lam (Scope () Exp a)
           | BinOp Op (Exp a) (Exp a)
           | App (Exp a) (Exp a)
           | IfZ (Exp a) (Exp a) (Exp a)
           deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

instance Eq1 Exp
instance Ord1 Exp
instance Read1 Exp
instance Show1 Exp

instance Applicative Exp where
    pure = Var
    (<*>) = ap

instance Monad Exp where
    return = Var
    (Var a)        >>= f = f a
    (NumLit n)     >>= _ = NumLit n
    (Lam e)        >>= f = Lam $ e >>>= f
    (BinOp op x y) >>= f = BinOp op (x >>= f) (y >>= f)
    (App x y)      >>= f = App (x >>= f) (y >>= f)
    (IfZ x y z)    >>= f = IfZ (x >>= f) (y >>= f) (z >>= f)

