{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-matches #-}
module Parser.Simple (top) where

-- Need to explicitly qualify Exp to avoid TH problems
import AST.Simple as AST
import Text.Peggy
import Bound
import Data.List (foldl')

chainl :: a -> [b] -> (a -> b -> a) -> a
chainl e xs f = foldl' f e xs

[peggy|
top :: AST.Exp String = expr !.

expr :: AST.Exp String
    = bigexpr
    / sumexpr

bigexpr :: AST.Exp String
    = "if" expr "then" expr "else" expr { IfZ $1 $2 $3 }
    / "\\" var "->" expr { Lam (abstract1 $1 $2) }

sumexpr :: AST.Exp String
    = mulexpr (sumop mulexpr)* { chainl $1 $2 (\x (op, y) -> BinOp op x y) }

mulexpr :: AST.Exp String
    = appexpr (mulop appexpr)* { chainl $1 $2 (\x (op, y) -> BinOp op x y) }

appexpr :: AST.Exp String
    = primexpr (primexpr)* { chainl $1 $2 App }

primexpr :: AST.Exp String
    = "(" expr ")"
    / space? int space? { IntLit $2 }
    / space? var space? { Var $2 }

int :: Int
    = [1-9] [0-9]* { read ($1 : $2) }
    / "0" { 0 }

var :: String
    = !keyword [a-zA-Z_] [a-zA-Z_0-9]* { $1 : $2 }

mulop :: Op
    = "*" { Times }
    / "/" { Divide }

sumop :: Op
    = "+" { Plus }
    / "-" { Minus }

keyword :: ()
    = "if"
    / "then"
    / "else"
|]
