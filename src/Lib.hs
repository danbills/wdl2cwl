{-# LANGUAGE DuplicateRecordFields #-}
module Lib
    ( someFunc, workflowOutputsParser
    ) where

import Control.Monad (void)
import Data.List
import Control.Monad.Combinators.Expr
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data BExpr
  = BoolConst Bool
  | Not BExpr
  | BBinary BBinOp BExpr BExpr
  | RBinary RBinOp AExpr AExpr
  deriving (Show)

data BBinOp
  = And
  | Or
  deriving (Show)

data RBinOp
  = Greater
  | Less
  deriving (Show)

data AExpr
  = Var String
  | IntConst Integer
  | Neg AExpr
  | ABinary ABinOp AExpr AExpr
  deriving (Show)

data ABinOp
  = Add
  | Subtract
  | Multiply
  | Divide
  deriving (Show)

data Stmt
  = Seq [Stmt]
  | Assign String AExpr
  | If BExpr Stmt Stmt
  | While BExpr Stmt
  | Skip
  deriving (Show)

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "#"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

-- | 'parens' parses something between parenthesis.

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | 'integer' parses an integer.

integer :: Parser Integer
integer = lexeme L.decimal

-- | 'semi' parses a semicolon.

semi :: Parser String
semi = symbol ";"

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

rws :: [String] -- list of reserved words
rws = ["if","then","else","while","do","skip","true","false","not","and","or"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

data WdlDeclaration = WdlType {
                                 _type :: WdlType,
                                 name :: String
                              }


whileParser :: Parser Stmt
whileParser = between sc eof stmt

stmt :: Parser Stmt
stmt = f <$> sepBy1 stmt' semi
  where
    -- if there's only one stmt return it without using ‘Seq’
    f l = if length l == 1 then head l else Seq l

stmt' :: Parser Stmt
stmt' = ifStmt
  <|> whileStmt
  <|> skipStmt
  <|> assignStmt
  <|> parens stmt

ifStmt :: Parser Stmt
ifStmt = do
  rword "if"
  cond  <- bExpr
  rword "then"
  stmt1 <- stmt
  rword "else"
  stmt2 <- stmt
  return (If cond stmt1 stmt2)

whileStmt :: Parser Stmt
whileStmt = do
  rword "while"
  cond <- bExpr
  rword "do"
  stmt1 <- stmt
  return (While cond stmt1)

assignStmt :: Parser Stmt
assignStmt = do
  var  <- identifier
  void (symbol ":=")
  expr <- aExpr
  return (Assign var expr)

skipStmt :: Parser Stmt
skipStmt = Skip <$ rword "skip"

aExpr :: Parser AExpr
aExpr = makeExprParser aTerm aOperators

bExpr :: Parser BExpr
bExpr = makeExprParser bTerm bOperators

aOperators :: [[Operator Parser AExpr]]
aOperators =
  [ [Prefix (Neg <$ symbol "-") ]
  , [ InfixL (ABinary Multiply <$ symbol "*")
    , InfixL (ABinary Divide   <$ symbol "/") ]
  , [ InfixL (ABinary Add      <$ symbol "+")
    , InfixL (ABinary Subtract <$ symbol "-") ]
  ]

bOperators :: [[Operator Parser BExpr]]
bOperators =
  [ [Prefix (Not <$ rword "not") ]
  , [InfixL (BBinary And <$ rword "and")
    , InfixL (BBinary Or <$ rword "or") ]
  ]

aTerm :: Parser AExpr
aTerm = parens aExpr
  <|> Var      <$> identifier
  <|> IntConst <$> integer

bTerm :: Parser BExpr
bTerm =  parens bExpr
  <|> (BoolConst True  <$ rword "true")
  <|> (BoolConst False <$ rword "false")
  <|> rExpr

rExpr :: Parser BExpr
rExpr = do
  a1 <- aExpr
  op <- relation
  a2 <- aExpr
  return (RBinary op a1 a2)

relation :: Parser RBinOp
relation = (Greater <$ symbol ">")
  <|> (Less <$ symbol "<")


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

--wfParser :: Parser Workflow


data Call = Call {
  name :: String
                 }

--     $wf_body_element = $call | $declaration | $while_loop | $if_stmt | $scatter | $wf_outputs | $wf_parameter_meta | $wf_meta
data WorkflowBodyElement = CallElement (Call) | WorkflowOutputsElement (WorkflowOutputs)

workflowBodyElementParser :: Parser WorkflowBodyElement
workflowBodyElementParser = WorkflowOutputsElement <$> workflowOutputsParser <|> CallElement <$> callParser


callParser :: Parser Call
callParser = Call <$> (call *> many alphaNumChar)


workflowOutputsParser :: Parser WorkflowOutputs
workflowOutputsParser = WorkflowOutputs <$> (output *> left_brace *> many (many alphaNumChar) <* right_brace)


data WorkflowOutputs = WorkflowOutputs {
  outputs :: [String]
                                       }

instance Show WorkflowOutputs where
    show wo = head (outputs wo)



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

data Wf_Or_Task_Or_Declaration =
    WorkflowDeclaration (Workflow)
                | Task
                | Declaration
