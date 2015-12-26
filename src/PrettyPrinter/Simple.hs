module PrettyPrinter.Simple where

import Text.PrettyPrint
import AST.Simple

ppPrimOp :: PrimOp -> Doc
ppPrimOp App = empty
ppPrimOp Add = char '+'
ppPrimOp Sub = char '-'
ppPrimOp Eq = text "=="
ppPrimOp Bind = text ">>="

ppExp :: Exp String -> Doc
ppExp (Var x) = text x
ppExp (Lit i) = int i
ppExp (Op op l r) = parens $ ppExp l <+> ppPrimOp op <+> ppExp r
ppExp (Let a e1 e2) = parens $ text "let" <+> text a <+> char '=' <+> ppExp e1 $$ text "in" <+> ppExp e2
ppExp (Lam a e) = parens $ char '\\' <> text a <+> text "->" <+> ppExp e
