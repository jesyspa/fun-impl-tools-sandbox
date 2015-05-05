module PrettyPrinter.SimpleCPS (pprintCPS) where

import AST.Simple
import AST.SimpleCPS
import Text.PrettyPrint

pprintOp :: Op -> Doc
pprintOp Plus = char '+'
pprintOp Minus = char '-'
pprintOp Times = char '*'
pprintOp Divide = char '/'

pprintLB :: LetBinder Doc -> Doc
pprintLB (OpBind op fe1 fe2) = pprintFE fe1 <+> pprintOp op <+> pprintFE fe2
pprintLB (ProjL v) = text "$projL" <+> v
pprintLB (ProjR v) = text "$projR" <+> v
pprintLB (Pair fe1 fe2) = text "$pair" <+> pprintFE fe1 <+> pprintFE fe2

pprintFE :: FlatExp Doc -> Doc
pprintFE (CIntLit n) = int n
pprintFE (CVar v) = v
pprintFE (CLam v e) = parens $ char '\\' <> v <+> text "->" <+> pprintCExp e

pprintCExp :: CExp Doc -> Doc
pprintCExp (Let v b e) = text "let" <+> v <+> char '=' <+> pprintLB b $$ text "in" $$ nest 4 (pprintCExp e)
pprintCExp (CIf fe e1 e2) = text "if" <+> pprintFE fe $+$ text "then" <+> pprintCExp e1 $+$ text "else" <+> pprintCExp e2
pprintCExp (Jump fe1 fe2) = text "$jump" <+> pprintFE fe1 <+> brackets (pprintFE fe2)
pprintCExp (Halt fe) = text "$halt" <+> pprintFE fe

pprintCPS :: CExp String -> String
pprintCPS x = render . pprintCExp $ text <$> x
