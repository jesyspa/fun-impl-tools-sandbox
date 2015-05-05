module AST.Simple where

import Bound
import Control.Monad (ap)
import Prelude.Extras

data Op = Plus | Minus | Times | Divide
        deriving (Eq, Ord, Read, Show)

data AST a = Var a
           | NumLit Int
           | Lam (Scope () AST a)
           | BinOp Op (AST a) (AST a)
           | App (AST a) (AST a)
           | IfZ (AST a) (AST a) (AST a)
           deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

instance Eq1 AST
instance Ord1 AST
instance Read1 AST
instance Show1 AST

instance Applicative AST where
    pure = Var
    (<*>) = ap

instance Monad AST where
    return = Var
    (Var a)        >>= f = f a
    (NumLit n)     >>= _ = NumLit n
    (Lam e)        >>= f = Lam $ e >>>= f
    (BinOp op x y) >>= f = BinOp op (x >>= f) (y >>= f)
    (App x y)      >>= f = App (x >>= f) (y >>= f)
    (IfZ x y z)    >>= f = IfZ (x >>= f) (y >>= f) (z >>= f)

