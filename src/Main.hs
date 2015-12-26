import AST.Simple
import Parser.Simple
import AST.GraphInst
import Interpreter.GraphReductionLang
import PrettyPrinter.Simple
import Text.PrettyPrint

main = do
    s <- getContents
    case parseExp s of
        Left err -> print err
        Right e -> putStrLn . render . ppExp $ e
