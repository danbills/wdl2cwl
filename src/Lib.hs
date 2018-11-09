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

importParser :: Parser WdlImport
importParser = WdlImport <$> (_import *> lexeme (many alphaNumChar))

wfParser :: Parser Workflow
wfParser = (\x y -> Workflow { name = x, body = y }) <$> nameParser <*> workflowSecondPart

nameParser :: Parser String
nameParser = workflow *> wdlName

workflowSecondPart :: Parser [WorkflowBodyElement]
workflowSecondPart = between left_brace right_brace (many workflowBodyElementParser)

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

worklowOutputElement = lexeme (many (letterChar <|> char '.'))

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
                         deriving (Show)

data Wf_Or_Task_Or_Declaration =
    WorkflowDeclaration (Workflow)
                | Task
                | Declaration

{-

task hello {
  String addressee
  command {
    echo "Hello ${addressee}!"
  }
  output {
    String salutation = read_string(stdout())
  }
  runtime {
    docker: "ubuntu@sha256:71cd81252a3563a03ad8daee81047b62ab5d892ebbfbf71cf53415f29c130950"
  }
}
-}

data OutputFileDescriptors = StdIn | StdOut

data TaskOutput = TaskOutput [TaskOutputDeclaration]

data TaskOutputDeclaration = TaskOutputDeclaration {
                                                      name :: String,
                                                      expression :: String
                                                   }
