module PrettyPrinter.Simple (pprint) where

import Bound
import Control.Monad.State (evalState)
import Text.PrettyPrint
import AST.Simple hiding (App)
import qualified AST.Simple as AST (AST(App))
import Tools.Gen
import Control.Applicative

data ParenLevel = None | Sum | Mul | App
                deriving (Eq, Ord, Read, Show)

parensIf :: Bool -> Doc -> Doc
parensIf True x = parens x
parensIf False x = x

infixl 4 <*+*>
(<*+*>) :: Applicative f => f Doc -> f Doc -> f Doc
(<*+*>) = liftA2 (<+>)

char' :: Applicative f => Char -> f Doc
char' = pure . char
text' :: Applicative f => String -> f Doc
text' = pure . text
int' :: Applicative f => Int -> f Doc
int' = pure . int

showLambda :: Scope () AST Doc -> Gen Int Doc
showLambda e = do
    n <- fresh
    let name = text "$x_" <> int n
        e' = instantiate1 (Var name) e
    body <- pprintPL None e'
    return $ char '\\' <> name <+> text "->" <+> body

pprintPL :: ParenLevel -> AST Doc -> Gen Int Doc
pprintPL _ (Var a) = pure a
pprintPL _ (NumLit n) = int' n
pprintPL l (Lam e) = parensIf (l > None) <$> showLambda e
pprintPL l (BinOp Plus x y) =  parensIf (l > None) <$> pNone x <*+*> char' '+' <*+*> pprintPL Sum y
pprintPL l (BinOp Minus x y) =  parensIf (l > None) <$> pNone x <*+*> char' '-' <*+*> pprintPL Sum y
pprintPL l (BinOp Times x y) =  parensIf (l > Sum) <$> pprintPL Sum x <*+*> char' '*' <*+*> pprintPL Mul y
pprintPL l (BinOp Divide x y) = parensIf (l > Sum) <$> pprintPL Sum x <*+*> char' '/' <*+*> pprintPL Mul y
pprintPL l (AST.App x y) = parensIf (l > Mul) <$> pprintPL Mul x <*+*> pprintPL App y
pprintPL l (IfZ x y z) = parensIf (l > None) <$> text' "if" <*+*> pNone x <*+*> text' "then" <*+*> pNone y <*+*> text' "else" <*+*> pNone z

pNone :: AST Doc -> Gen Int Doc
pNone = pprintPL None

pprint :: AST String -> String
pprint x = render $ evalState (pNone $ text <$> x) 0

