module Compiler.CPS(compile) where

-- Based on http://jozefg.bitbucket.org/posts/2015-04-30-cps.html

import AST.Simple
import AST.SimpleCPS
import Bound
import Control.Monad
import Control.Monad.Gen

compile :: (Eq a, Enum a) => Exp a -> CExp a
compile = runGen . flip cps (return . Halt)

cps :: (Eq a, Enum a) => Exp a -> (FlatExp a -> Gen a (CExp a)) -> Gen a (CExp a)
cps (Var v) c = c (CVar v)
cps (IntLit n) c = c (CIntLit n)
cps (IfZ i x y) c = cps i $ \ic -> CIf ic <$> cps x c <*> cps y c
cps (BinOp op l r) c =
    cps l $ \fl ->
    cps r $ \fr ->
    gen >>= \out ->
    Let out (OpBind op fl fr) <$> c (CVar out)
cps (Lam body) c = do
    [pairArg, newCont, newArg] <- replicateM 3 gen
    let body' = instantiate1 (Var newArg) body
    cbody <- cps body' (return . Jump (CVar newCont))
    c (CLam pairArg
        $ Let newArg (ProjL pairArg)
        $ Let newCont (ProjR pairArg)
        $ cbody)
cps (App l r) c = do
    arg <- gen
    cont <- CLam arg <$> c (CVar arg)
    cps l $ \fl ->
        cps r $ \fr ->
        gen >>= \pair -> 
        return $ Let pair (Pair fr cont) (Jump fl (CVar pair))

