import Parser.Simple
import PrettyPrinter.Simple
import Text.Peggy
import Control.Monad

handleParseError :: (a -> IO ()) -> Either ParseError a -> IO ()
handleParseError _ (Left err) = print err
handleParseError go (Right a) = go a

main :: IO ()
main = forever $ handleParseError (putStrLn . pprint) . parseString top "<stdin>" =<< getLine
