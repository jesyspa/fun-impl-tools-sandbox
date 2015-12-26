module Interpreter.GraphReductionLang where

import qualified Data.Map as M
import Data.IORef
import Control.Monad.Reader
import AST.GraphInst
import Interpreter.DAG

type SymbolsReaderT a = ReaderT (M.Map a [Inst a])

unroll' :: DAG a -> [DAGRef a] -> IO (Cmd a, [DAGRef a])
unroll' (Cmd a) xs = return (a, xs)
unroll' (Literal _) _ = error "cannot apply literal"
unroll' (App l r) xs = do
    l' <- readIORef l
    unroll' l' (r:xs)

unroll :: [DAGRef a] -> IO (Cmd a, [DAGRef a])
unroll (x:xs) = do
    x' <- readIORef x
    unroll' x' xs
unroll [] = error "empty spine"

push :: DAG a -> [DAGRef a] -> IO [DAGRef a]
push dag xs = (:xs) <$> newIORef dag

reportError :: Show a => String -> [DAGRef a] -> IO b
reportError op spn = do
    xs <- mapM (readIORef >=> printDAG) spn
    putStrLn $ "invalid application of " ++ op
    putStrLn "Spine:"
    forM_ xs putStrLn
    error "fatal error"

run' :: Show a => Inst a -> [DAGRef a] -> IO [DAGRef a]
run' (PushLit i) spn = push (Literal i) spn
run' (PushArg i) spn = return $ (spn !! i) : spn
run' (PushPrg a) spn = push (Cmd $ Prog a) spn
run' (PushPrim p) spn = push (Cmd $ Prim p) spn
run' MkApp (x:y:spn) = push (App x y) spn
run' Slide (x:_:spn) = return $ x : spn
run' op spn = reportError (show op) spn

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
runPrim op spn = lift $ reportError (show op) spn

eval' :: (Show a, Ord a) => [DAGRef a] -> Cmd a -> SymbolsReaderT a IO [DAGRef a]
eval' spn (Prog a) = ask >>= \st -> lift $ run (st M.! a) spn
eval' spn (Prim p) = runPrim p spn

evalExec :: (Show a, Ord a) => [DAGRef a] -> SymbolsReaderT a IO Int
evalExec dag = do
    (a, spn) <- lift $ unroll dag
    spn' <- eval' spn a
    eval spn'

eval :: (Show a, Ord a) => [DAGRef a] -> SymbolsReaderT a IO Int
eval [x] = do
    x' <- lift $ readIORef x
    case x' of
        Literal i -> return i
        _ -> evalExec [x]
eval spn = evalExec spn

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
