{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module GraphReductionLang where

import qualified Data.Map as M

data PrimOp = Add
            | Sub
            | Eq
            | Branch
            deriving (Eq, Ord, Read, Show)

data Inst a = PushLit Int
            | PushArg Int
            | PushPrg a
            | PushPrim PrimOp
            | MkApp
            | Slide
            deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

data Cmd a = Prog a
           | Prim PrimOp
           deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

infixl `App`

data DAG a = Cmd (Cmd a)
           | Literal Int
           | App (DAG a) (DAG a)
           deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

unroll' :: DAG a -> [DAG a] -> (Cmd a, [DAG a])
unroll' (Cmd a) xs = (a, xs)
unroll' (Literal _) _ = error "Cannot apply literal"
unroll' (App l r) xs = unroll' l (r:xs)

unroll :: DAG a -> (Cmd a, [DAG a])
unroll dag = unroll' dag []

roll :: [DAG a] -> DAG a
roll = foldl1 App

run' :: Show a => Inst a -> [DAG a] -> [DAG a]
run' (PushLit i) spn = Literal i : spn
run' (PushArg i) spn = (spn !! i) : spn
run' (PushPrg a) spn = Cmd (Prog a) : spn
run' (PushPrim p) spn = Cmd (Prim p) : spn
run' MkApp (x:y:spn) = App x y : spn
run' Slide (x:_:spn) = x : spn
run' op spn = error $ "cannot apply " ++ show op ++ " when stack is " ++ show spn

run :: Show a => [Inst a] -> [DAG a] -> [DAG a]
run is spn = foldl (\r i -> run' i r) spn is

runPrim :: (Show a, Ord a) => M.Map a [Inst a] -> PrimOp -> [DAG a] -> [DAG a]
runPrim st Add (x:y:spn) = Literal (eval st x + eval st y) : spn
runPrim st Sub (x:y:spn) = Literal (eval st x - eval st y) : spn
runPrim st Eq (x:y:spn) = if eval st x == eval st y then Literal 1 : spn else Literal 0 : spn
runPrim st Branch (x:y:z:spn) = if eval st x /= 0 then y : spn else z : spn
runPrim _ p spn = error $ "cannot apply " ++ show p ++ " when stack is " ++ show spn

eval' :: (Show a, Ord a) => M.Map a [Inst a] -> [DAG a] -> Cmd a -> DAG a
eval' st spn (Prog a) = roll $ run (st M.! a) spn
eval' st spn (Prim p) = roll $ runPrim st p spn

eval :: (Show a, Ord a) => M.Map a [Inst a] -> DAG a -> Int
eval st (Literal i) = i
eval st dag = eval st $ eval' st spn a
    where (a, spn) = unroll dag


