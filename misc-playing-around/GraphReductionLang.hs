{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module GraphReductionLang where

import qualified Data.Map as M
import Control.Monad.ST
import Data.STRef

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

infixl `App`

data DAG s a = Cmd (Cmd a)
             | Literal Int
             | App (DAGRef s a) (DAGRef s a)

type DAGRef s a = STRef s (DAG s a)

unroll' :: DAG s a -> [DAGRef s a] -> ST s (Cmd a, [DAGRef s a])
unroll' (Cmd a) xs = return (a, xs)
unroll' (Literal _) _ = error "Cannot apply literal"
unroll' (App l r) xs = do
    l' <- readSTRef l
    unroll' l' (r:xs)

unroll :: [DAGRef s a] -> ST s (Cmd a, [DAGRef s a])
unroll (x:xs) = do
    x' <- readSTRef x
    unroll' x' xs

run' :: Show a => Inst a -> [DAGRef s a] -> ST s [DAGRef s a]
run' (PushLit i) spn = do
    i' <- newSTRef (Literal i)
    return $ i' : spn
run' (PushArg i) spn = return $ (spn !! i) : spn
run' (PushPrg a) spn = do
    a' <- newSTRef $ Cmd (Prog a)
    return $ a' : spn
run' (PushPrim p) spn = do
    p' <- newSTRef $ Cmd (Prim p)
    return $ p' : spn
run' MkApp (x:y:spn) = do
    a <- newSTRef (App x y)
    return $ a : spn
run' Slide (x:_:spn) = return $ x : spn
run' op spn = error $ "cannot apply " ++ show op

run :: Show a => [Inst a] -> [DAGRef s a] -> ST s [DAGRef s a]
run is spn = foldl (>>=) (return spn) $ map run' is

runPrim :: (Show a, Ord a) => M.Map a [Inst a] -> PrimOp -> [DAGRef s a] -> ST s [DAGRef s a]
runPrim st Add (x:y:spn) = do
    x' <- eval st [x]
    y' <- eval st [y]
    i <- newSTRef (Literal $ x' + y')
    return $ i : spn
runPrim st Sub (x:y:spn) = do
    x' <- eval st [x]
    y' <- eval st [y]
    i <- newSTRef (Literal $ x' - y')
    return $ i : spn
runPrim st Eq (x:y:spn) = do
    x' <- eval st [x]
    y' <- eval st [y]
    i <- newSTRef $ if x' == y' then Literal 1 else Literal 0
    return $ i : spn
runPrim st Branch (x:y:z:spn) = do
    x' <- eval st [x]
    return $ if x' /= 0 then y : spn else z : spn
runPrim _ p spn = error $ "cannot apply " ++ show p

eval' :: (Show a, Ord a) => M.Map a [Inst a] -> [DAGRef s a] -> Cmd a -> ST s [DAGRef s a]
eval' st spn (Prog a) = run (st M.! a) spn
eval' st spn (Prim p) = runPrim st p spn

eval :: (Show a, Ord a) => M.Map a [Inst a] -> [DAGRef s a] -> ST s Int
eval st dag = do
    x <- readSTRef (head dag)
    case x of
        Literal i -> return i
        _ -> do
            (a, spn) <- unroll dag
            spn' <- eval' st spn a
            eval st spn'

evaluate :: M.Map String [Inst String] -> Int
evaluate st = runST $ do
    r <- newSTRef (Cmd (Prog "main"))
    eval st [r]

