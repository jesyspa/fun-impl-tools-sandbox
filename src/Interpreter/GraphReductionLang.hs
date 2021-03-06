module Interpreter.GraphReductionLang where

import qualified Data.Map as M
import Data.IORef
import Control.Monad.Reader
import AST.GraphInst
import Interpreter.DAG

type MonadSymbolsReader a m = (MonadIO m, MonadReader (M.Map a [Inst a]) m, Ord a, Show a)

unroll' :: MonadIO m => DAG a -> [DAGRef a] -> m (Cmd a, [DAGRef a])
unroll' (Cmd a) xs = return (a, xs)
unroll' (Literal _) _ = error "cannot apply literal"
unroll' (App l r) xs = do
    l' <- liftIO $ readIORef l
    unroll' l' (r:xs)

unroll :: MonadIO m => [DAGRef a] -> m (Cmd a, [DAGRef a])
unroll (x:xs) = do
    x' <- liftIO $ readIORef x
    unroll' x' xs
unroll [] = error "empty spine"

push :: MonadIO m => DAG a -> [DAGRef a] -> m [DAGRef a]
push dag xs = (:xs) <$> liftIO (newIORef dag)

reportError :: (MonadIO m, Show a) => String -> [DAGRef a] -> m b
reportError op spn = do
    xs <- mapM (liftIO . readIORef >=> printDAG) spn
    liftIO . putStrLn $ "invalid application of " ++ op
    liftIO $ putStrLn "Spine:"
    forM_ xs $ liftIO . putStrLn
    error "fatal error"

run' :: (MonadIO m, Show a) => Inst a -> [DAGRef a] -> m [DAGRef a]
run' (PushLit i) spn = push (Literal i) spn
run' (PushArg i) spn = return $ (spn !! i) : spn
run' (PushPrg a) spn = push (Cmd $ Prog a) spn
run' (PushPrim p) spn = push (Cmd $ Prim p) spn
run' MkApp (x:y:spn) = push (App x y) spn
run' Slide (x:_:spn) = return $ x : spn
run' op spn = reportError (show op) spn

run :: (MonadIO m, Show a) => [Inst a] -> [DAGRef a] -> m [DAGRef a]
run is spn = foldl (>>=) (return spn) $ map run' is

runPrim :: MonadSymbolsReader a m => PrimOp -> [DAGRef a] -> m [DAGRef a]
runPrim Add (x:y:spn) = do
    x' <- evalUpdate x
    y' <- evalUpdate y
    push (Literal $ x' + y') spn
runPrim Sub (x:y:spn) = do
    x' <- evalUpdate x
    y' <- evalUpdate y
    push (Literal $ x' - y') spn
runPrim Eq (x:y:spn) = do
    x' <- evalUpdate x
    y' <- evalUpdate y
    push (Literal $ if x' == y' then 1 else 0) spn
runPrim Branch (x:y:z:spn) = do
    x' <- evalUpdate x
    return $ if x' /= 0 then y : spn else z : spn
runPrim Read spn = do
    x <- liftIO $ readLn
    push (Literal x) spn
runPrim Print (x:spn) = do
    x' <- evalUpdate x
    liftIO $ print x'
    return $ x : spn
runPrim Bind (x:y:spn) = do
    _ <- evalUpdate x
    return $ y:x:spn
runPrim op spn = reportError (show op) spn

eval' :: MonadSymbolsReader a m => [DAGRef a] -> Cmd a -> m [DAGRef a]
eval' spn (Prog a) = ask >>= \st -> run (st M.! a) spn
eval' spn (Prim p) = runPrim p spn

evalExec :: MonadSymbolsReader a m => [DAGRef a] -> m Int
evalExec dag = do
    (a, spn) <- unroll dag
    spn' <- eval' spn a
    eval spn'

eval :: MonadSymbolsReader a m => [DAGRef a] -> m Int
eval [x] = do
    x' <- liftIO $ readIORef x
    case x' of
        Literal i -> return i
        _ -> evalExec [x]
eval spn = evalExec spn

evalUpdate :: MonadSymbolsReader a m => DAGRef a -> m Int
evalUpdate dag = do
    x <- eval [dag]
    liftIO $ writeIORef dag (Literal x)
    return x

evaluate :: M.Map String [Inst String] -> String -> IO Int
evaluate st entry = flip runReaderT st $ do
    r <- lift $ newIORef (Cmd (Prog entry))
    evalUpdate r

evaluate' :: [Inst String] -> IO Int
evaluate' xs = evaluate (M.fromList [("main", xs)]) "main"

testCode = [PushLit 1] ++ concat (replicate 20 [PushArg 0, PushArg 1, PushPrim Add, MkApp, MkApp, Slide])
