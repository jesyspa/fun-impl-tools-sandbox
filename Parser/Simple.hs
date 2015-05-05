{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-matches #-}
module Parser.Simple (top) where

import AST.Simple
import Text.Peggy
import Bound

chainl :: a -> [b] -> (a -> b -> a) -> a
chainl e (x:xs) f = chainl (f e x) xs f
chainl e [] _ = e

[peggy|
top :: AST String = expr !.

expr :: AST String
    = bigexpr
    / sumexpr

bigexpr :: AST String
    = "if" expr "then" expr "else" expr { IfZ $1 $2 $3 }
    / "\\" var "->" expr { Lam (abstract1 $1 $2) }

sumexpr :: AST String
    = mulexpr (sumop mulexpr)* { chainl $1 $2 (\x (op, y) -> BinOp op x y) }

mulexpr :: AST String
    = appexpr (mulop appexpr)* { chainl $1 $2 (\x (op, y) -> BinOp op x y) }

appexpr :: AST String
    = primexpr (primexpr)* { chainl $1 $2 App }

primexpr :: AST String
    = "(" expr ")"
    / space? num space? { NumLit $2 }
    / space? var space? { Var $2 }

num :: Int
    = [1-9] [0-9]* { read ($1 : $2) }

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
