import Parser.Simple
import Text.Peggy
import Control.Monad

main :: IO ()
main = forever $ print . parseString top "<stdin>" =<< getLine
