import AST.Simple
import AST.SimpleCPS
import Parser.Simple
import PrettyPrinter.Simple
import PrettyPrinter.SimpleCPS
import Compiler.CPS as CPS
import Interpreter.CPS as ICPS
import Text.Peggy
import Control.Monad
import Bound

ifSuccessful :: (a -> IO ()) -> Either ParseError a -> IO ()
ifSuccessful _ (Left err) = print err
ifSuccessful go (Right a) = go a

prettyEcho :: Exp String -> IO ()
prettyEcho = putStrLn . pprint

compileCPS :: Exp String -> IO ()
compileCPS xs = case closed xs of
                    Just e -> putStrLn . pprintCPS . makeNamesCPS $ CPS.compile e
                    Nothing -> print "error: free variable detected"

runCPS :: Exp String -> IO ()
runCPS xs = case closed xs of
                Just e -> print . ICPS.eval . makeNamesCPS $ CPS.compile e
                Nothing -> print "error: free variable detected"

main :: IO ()
main = forever $ ifSuccessful runCPS . parseString top "<stdin>" =<< getLine
