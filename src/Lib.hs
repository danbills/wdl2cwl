{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Lib
    --( someFunc, workflowOutputsParser, stringsParser, testParser, sc) where
     where

import GHC.Generics
import Control.Monad (void)
import Debug.Trace
import Data.List
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "#"
    blockCmnt = L.skipBlockComment "/*" "*/"

sc2 :: Parser ()
sc2 = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "#"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = try . L.lexeme sc

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

data WdlDeclaration = WdlType {
                                 _type :: WdlType,
                                 name :: String
                              }

{-

wdlParser :: Parser WdlStmt
wdlParser = between sc eof wdlStmt

wdlStmt :: Parser WdlStmt
wdlStmt = WdlStmt <$> many importParser <*> many wf_or_task_or_declaration

wf_or_task_or_declaration :: Parser Wf_Or_Task_Or_Declaration
wf_or_task_or_declaration = wfParser <|> taskParser -- needs decl
-}

importParser :: Parser WdlImport
importParser = WdlImport <$> (_import *> lexeme (many alphaNumChar))


wdlType = rword "File"

wfParser :: Parser Workflow
wfParser = (\x y -> Workflow { name = x, body = y }) <$> nameParser <*> workflowSecondPart

nameParser :: Parser String
nameParser = workflow *> wdlName

workflowSecondPart :: Parser [WorkflowBodyElement]
workflowSecondPart = between left_brace right_brace (many (workflowBodyElementParser <* traceM "parsed something"))

data Call = Call {
  name :: String
                 }
                 deriving (Show)

--     $wf_body_element = $call | $declaration | $while_loop | $if_stmt | $scatter | $wf_outputs | $wf_parameter_meta | $wf_meta
data WorkflowBodyElement = CallElement (Call) | WorkflowOutputsElement (WorkflowOutputs)
  deriving (Show)

workflowBodyElementParser :: Parser WorkflowBodyElement
workflowBodyElementParser = WorkflowOutputsElement <$> workflowOutputsParser <|> CallElement <$> callParser

callParser :: Parser Call
callParser = Call <$> (call *> wdlName)

workflowOutputsParser :: Parser WorkflowOutputs
workflowOutputsParser = WorkflowOutputs <$> (output *> left_brace *>  manyTill worklowOutputElement right_brace)

worklowOutputElement = lexeme (try (many (letterChar <|> char '.')))

wdlName :: Parser String
wdlName = lexeme (many (letterChar <|> char '_'))

stringsParser :: Parser String
stringsParser = lexeme (many letterChar)

testParser =  manyTill stringsParser eof

data WorkflowOutputs = WorkflowOutputs {
  outputs :: [String]
                                       }
                                       deriving (Show)

data WdlStmt = WdlStmt {
                        imports :: [WdlImport],
                        wf_or_task_or_declarations :: [Wf_Or_Task_Or_Declaration]
                       }

data WdlImport = WdlImport String

task = rword "task"
workflow = rword "workflow"
_import = rword "import"
call = rword "call"
input = rword "input"
output = rword "output"
as = rword "as"
_if = rword "if"
_then = rword "then"
_else = rword "else"
while = rword "while"
runtime = rword "runtime"
scatter = rword "scatter"
command = rword "command"
parameter_meta = rword "parameter_meta"
meta = rword "meta"
true = rword "true"
false = rword "false"
object = rword "object"
left_brace = rword "{"
right_brace = rword "}"

data WdlType =
    WdlFile
    | Array
    | Map
    | Object
    | Pair
    | Boolean
    | Int
    | Float
    | Uri
    | File
    | String

data Workflow = Workflow {
name :: String,
body :: [WorkflowBodyElement]
                         }
                         deriving (Generic, Show)

data Wf_Or_Task_Or_Declaration =
    WorkflowDeclaration (Workflow)
                | Task
                | Declaration
