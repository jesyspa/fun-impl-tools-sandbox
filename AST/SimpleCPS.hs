module AST.SimpleCPS where

-- Based on http://jozefg.bitbucket.org/posts/2015-04-30-cps.html

import AST.Simple (Op)

data LetBinder a = OpBind Op (FlatExp a) (FlatExp a)
                 | ProjL a
                 | ProjR a
                 | Pair (FlatExp a) (FlatExp a)
                 deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

data FlatExp a = CIntLit Int
               | CVar a
               | CLam a (CExp a)
               deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

data CExp a = Let a (LetBinder a) (CExp a)
            | CIf (FlatExp a) (CExp a) (CExp a)
            | Jump (FlatExp a) (FlatExp a)
            | Halt (FlatExp a)
            deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

makeNamesCPS :: CExp Int -> CExp String
makeNamesCPS = fmap (\x -> "$x_" ++ show x)
