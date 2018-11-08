import System.Exit
import Lib
import Text.Megaparsec

{-
main :: IO ()
main = do
    contents <- readFile "test/hello.wdl"
    putStrLn contents
    parseTest wdlParser contents
    exitWith (ExitFailure 1)
    -}

main :: IO ()
main = do
    contents <- readFile "test/output.txt"
    putStrLn contents
    parseTest workflowOutputsParser contents
