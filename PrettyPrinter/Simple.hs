module PrettyPrinter.Simple (pprint) where

import Text.PrettyPrint
import AST.Simple hiding (App)
import qualified AST.Simple as AST (AST(App))

data ParenLevel = None | Sum | Mul | App
                deriving (Eq, Ord, Read, Show)

parensIf :: Bool -> Doc -> Doc
parensIf True x = parens x
parensIf False x = x

pprintPL :: ParenLevel -> AST String -> Doc
pprintPL _ (Var a) = text a
pprintPL _ (NumLit n) = int n
pprintPL _ (Lam _) = text "lambda"
pprintPL l (BinOp Plus x y) = parensIf (l > None) $ pprintPL None x <+> char '+' <+> pprintPL Sum y
pprintPL l (BinOp Minus x y) = parensIf (l > None) $ pprintPL None x <+> char '-' <+> pprintPL Sum y
pprintPL l (BinOp Times x y) = parensIf (l > Sum) $ pprintPL Sum x <+> char '*' <+> pprintPL Mul y
pprintPL l (BinOp Divide x y) = parensIf (l > Sum) $ pprintPL Sum x <+> char '/' <+> pprintPL Mul y
pprintPL l (AST.App x y) = parensIf (l > Mul) $ pprintPL Mul x <+> pprintPL App y
pprintPL l (IfZ x y z) = parensIf (l > None) $ text "if" <+> pprintPL None x <+> text "then" <+> pprintPL None y <+> text "else" <+> pprintPL None z

pprint :: AST String -> String
pprint = render . pprintPL None

