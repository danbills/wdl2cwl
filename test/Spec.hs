import Lib
import Text.Megaparsec
import Text.Megaparsec.Char

{-
main :: IO ()
main = do
    contents <- readFile "test/hello.wdl"
    putStrLn contents
    parseTest wdlParser contents
    exitWith (ExitFailure 1)
    -}

p1 :: Parser String
p1 = between sc eof (lexeme (string "object"))

t2 = do
    parseTest p1 "object"

t3 = do
    contents <- readFile "test/workflow.txt"
    parseTest (between sc eof wfParser ) contents


t1 :: IO ()
t1 = do
    contents <- readFile "test/output.txt"
    parseTest (between sc eof workflowOutputsParser ) contents

t4 :: IO()
t4 = parseTest worklowOutputElement "some.thing"

main :: IO ()
main = do
    t2
    t1
    t3
    t4
