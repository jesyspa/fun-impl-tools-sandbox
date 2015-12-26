module AST.GraphInst where

data PrimOp = Add
            | Sub
            | Eq
            | Branch
            | Read
            | Print
            | Bind
            deriving (Eq, Ord, Read, Show)

data Inst a = PushLit Int
            | PushArg Int
            | PushPrg a
            | PushPrim PrimOp
            | MkApp
            | Slide
            deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)
