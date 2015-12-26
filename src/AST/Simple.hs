module AST.Simple where

data PrimOp = App
            | Add
            | Sub
            | Eq
            | Bind
            deriving (Eq, Ord, Read, Show)

data Exp a = Var a
           | Lit Int
           | Op PrimOp (Exp a) (Exp a)
           | Let a (Exp a) (Exp a)
           | Lam a (Exp a)
           deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

