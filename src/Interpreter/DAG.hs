module Interpreter.DAG where

import Data.IORef
import AST.GraphInst

data Cmd a = Prog a
           | Prim PrimOp
           deriving (Eq, Ord, Read, Show)

infixl `App`

data DAG a = Cmd (Cmd a)
           | Literal Int
           | App (DAGRef a) (DAGRef a)

type DAGRef a = IORef (DAG a)

printDAG :: Show a => DAG a -> IO String
printDAG (Cmd cmd) = return $ show cmd
printDAG (Literal i) = return $ show i
printDAG (App l r) = do
    l' <- readIORef l >>= printDAG
    r' <- readIORef r >>= printDAG
    return $ "(" ++ l' ++ " " ++ r' ++ ")"
