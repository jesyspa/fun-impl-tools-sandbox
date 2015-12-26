module Parser.Simple where

import Control.Monad.Identity
import Text.Parsec
import Text.Parsec.Expr
import Control.Applicative ((<$>), (<*))
import AST.Simple

pKeyword :: String -> Parsec String () ()
pKeyword s = string s *> spaces

pSymbol :: String -> Parsec String () ()
pSymbol s = string s *> spaces

pVariable :: Parsec String () String
pVariable = many1 alphaNum <* spaces

pLiteral :: Parsec String () Int
pLiteral = read <$> many1 digit <* spaces

binary :: PrimOp -> String -> Operator String () Identity (Exp String)
binary op sm = Infix (Op op <$ pSymbol sm) AssocLeft

operators = [ [binary App ""]
            , [binary Add "+", binary Sub "-"]
            , [binary Eq "=="]
            , [binary Bind ">>="]
            ]

pSimpleExp :: Parsec String () (Exp String)
pSimpleExp = pSymbol "(" *> pExp <* pSymbol ")" <|> Lit <$> pLiteral <|> Var <$> pVariable

pOpExp :: Parsec String () (Exp String)
pOpExp = buildExpressionParser operators pSimpleExp

pLambda :: Parsec String () (Exp String)
pLambda = Lam <$ pSymbol "\\" <*> pVariable <* pSymbol "->" <*> pExp

pLet :: Parsec String () (Exp String)
pLet = Let <$ pKeyword "let" <*> pVariable <* pSymbol "=" <*> pExp <* pKeyword "in" <*> pExp

pExp :: Parsec String () (Exp String)
pExp = pLambda <|> pLet <|> pOpExp

parseExp :: String -> Either ParseError (Exp String)
parseExp = parse pExp ""
