module Interpreter.CPS where

import AST.SimpleCPS
import AST.Simple
import qualified Data.Map as M
import Control.Applicative
import Control.Monad.Reader
import Prelude hiding (lookup)

-- Parametrised CPS-value
type PCValue a = Reader (M.Map a (CValue a)) (CValue a)

data CValue a = CInt Int
              | CPair (CValue a) (CValue a)
              | CFun a (CExp a)
              deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

lookup :: Ord a => a -> PCValue a
lookup name = (M.!name) <$> ask

toValue :: Ord a => FlatExp a -> PCValue a
toValue (CIntLit n) = return $ CInt n
toValue (CVar a) = lookup a
toValue (CLam a e) = return $ CFun a e

calc :: Ord a => LetBinder a -> PCValue a
calc (OpBind op x y) = do
    CInt x' <- toValue x
    CInt y' <- toValue y
    return $ case op of
               Plus -> CInt (x' + y')
               Minus -> CInt (x' - y')
               Times -> CInt (x' * y')
               Divide -> CInt (x' `div` y')
calc (ProjL x) = (\(CPair a _) -> a) <$> lookup x
calc (ProjR x) = (\(CPair _ b) -> b) <$> lookup x
calc (Pair x y) = CPair <$> toValue x <*> toValue y

evalC :: Ord a => CExp a -> PCValue a
evalC (Let a e body) = calc e >>= \x -> local (M.insert a x) (evalC body)
evalC (CIf i x y) = toValue i >>= \(CInt v) -> if v == 0 then evalC x else evalC y
evalC (Jump x y) = do
    CFun a body <- toValue x
    y' <- toValue y
    local (M.insert a y') (evalC body)
evalC (Halt x) = toValue x

eval :: Ord a => CExp a -> CValue a
eval x = runReader (evalC x) M.empty

