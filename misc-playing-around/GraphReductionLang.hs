{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module GraphReductionLang where

import qualified Data.Map as M
import Data.IORef
import Control.Monad.Reader

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

data Cmd a = Prog a
           | Prim PrimOp

infixl `App`

data DAG a = Cmd (Cmd a)
           | Literal Int
           | App (DAGRef a) (DAGRef a)

type DAGRef a = IORef (DAG a)

type SymbolsReaderT a = ReaderT (M.Map a [Inst a])

unroll' :: DAG a -> [DAGRef a] -> IO (Cmd a, [DAGRef a])
unroll' (Cmd a) xs = return (a, xs)
unroll' (Literal _) _ = error "Cannot apply literal"
unroll' (App l r) xs = do
    l' <- readIORef l
    unroll' l' (r:xs)

unroll :: [DAGRef a] -> IO (Cmd a, [DAGRef a])
unroll (x:xs) = do
    x' <- readIORef x
    unroll' x' xs

run' :: Show a => Inst a -> [DAGRef a] -> IO [DAGRef a]
run' (PushLit i) spn = do
    i' <- newIORef (Literal i)
    return $ i' : spn
run' (PushArg i) spn = return $ (spn !! i) : spn
run' (PushPrg a) spn = do
    a' <- newIORef $ Cmd (Prog a)
    return $ a' : spn
run' (PushPrim p) spn = do
    p' <- newIORef $ Cmd (Prim p)
    return $ p' : spn
run' MkApp (x:y:spn) = do
    a <- newIORef (App x y)
    return $ a : spn
run' Slide (x:_:spn) = return $ x : spn
run' op spn = error $ "cannot apply " ++ show op

run :: Show a => [Inst a] -> [DAGRef a] -> IO [DAGRef a]
run is spn = foldl (>>=) (return spn) $ map run' is

runPrim :: (Show a, Ord a) => PrimOp -> [DAGRef a] -> SymbolsReaderT a IO [DAGRef a]
runPrim Add (x:y:spn) = do
    x' <- evalUpdate x
    y' <- evalUpdate y
    i <- lift $ newIORef (Literal $ x' + y')
    return $ i : spn
runPrim Sub (x:y:spn) = do
    x' <- evalUpdate x
    y' <- evalUpdate y
    i <- lift $ newIORef (Literal $ x' - y')
    return $ i : spn
runPrim Eq (x:y:spn) = do
    x' <- evalUpdate x
    y' <- evalUpdate y
    i <- lift $ newIORef $ if x' == y' then Literal 1 else Literal 0
    return $ i : spn
runPrim Branch (x:y:z:spn) = do
    x' <- evalUpdate x
    return $ if x' /= 0 then y : spn else z : spn
runPrim Read spn = do
    x <- lift $ readLn
    x' <- lift $ newIORef (Literal x)
    return $ x' : spn
runPrim Print (x:spn) = do
    x' <- evalUpdate x
    lift $ print x'
    return $ x : spn
runPrim Bind (x:y:spn) = do
    _ <- evalUpdate x
    return $ y:x:spn
runPrim p spn = error $ "cannot apply " ++ show p

eval' :: (Show a, Ord a) => [DAGRef a] -> Cmd a -> SymbolsReaderT a IO [DAGRef a]
eval' spn (Prog a) = ask >>= \st -> lift $ run (st M.! a) spn
eval' spn (Prim p) = runPrim p spn

eval :: (Show a, Ord a) => [DAGRef a] -> SymbolsReaderT a IO Int
eval dag = do
    x <- lift $ readIORef (head dag)
    case x of
        Literal i -> return i
        _ -> do
            (a, spn) <- lift $ unroll dag
            spn' <- eval' spn a
            eval spn'

evalUpdate :: (Show a, Ord a) => DAGRef a -> SymbolsReaderT a IO Int
evalUpdate dag = do
    x <- eval [dag]
    lift $ writeIORef dag (Literal x)
    return x

evaluate :: M.Map String [Inst String] -> String -> IO Int
evaluate st entry = flip runReaderT st $ do
    r <- lift $ newIORef (Cmd (Prog entry))
    evalUpdate r

evaluate' :: [Inst String] -> IO Int
evaluate' xs = evaluate (M.fromList [("main", xs)]) "main"

testCode = [PushLit 1, PushLit 1, PushPrim Add, MkApp, MkApp] ++ concat (replicate 20 [PushArg 0, PushArg 1, PushPrim Add, MkApp, MkApp])
