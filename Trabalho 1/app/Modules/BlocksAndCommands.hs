module BlocksAndCommands where

import ArithmeticExpressions (arithmeticExpression)
import Lexer (braces', commaSep', identifier', parens', reserved', reservedOp', semi', stringLiteral', whiteSpace')
import LogicalExpressions (logicalExpression, logicalExpression')
import RelationalExpressions (relationalExpression)
import Text.Parsec (Parsec, choice, many, many1, optionMaybe, optional, try, (<|>), sepBy, option)
import Types (Bloco, Comando (..), Expr (Chamada), ExprL (..), ExprR, Id, Var)
import Data.Maybe (fromMaybe)
import VariableDeclarations (expression, functionCall', variableDeclarations)
import Text.Parsec.Char (char)


-- Função principal para analisar blocos
block :: Parsec String () Bloco
block = braces' commandList

-- Função auxiliar para analisar blocos com declarações de variáveis
block' :: Parsec String () ([Var], Bloco)
block' = braces' $ do
  vars <- option [] (many (try variableDeclarations)) -- tenta analisar declarações de variáveis
  cmds <- try commandList
  return (concat vars, cmds)

-- Função auxiliar para analisar blocos com declarações de variáveis
block'' :: Parsec String () Bloco
block'' = braces' $ many (whiteSpace' *> command <* whiteSpace')

-- Função auxiliar para analisar listas de comandos
commandList :: Parsec String () [Comando]
commandList = many (whiteSpace' *> command <* whiteSpace')

-- Função principal para analisar comandos
command :: Parsec String () Comando
command =
  choice
    [ 
      try atribCommand,
      try ifCommand,
      try whileCommand,
      try readCommand,
      try printCommand,
      try returnCommand,
      try functionCall
    ]

-- Função auxiliar para analisar comandos If
ifCommand :: Parsec String () Comando
ifCommand = do
  reserved' "if"
  cond <-
    try (parens' logicalExpression)
      <|> try (parens' $ Rel <$> relationalExpression)
      <|> try (Rel <$> relationalExpression)

  trueBlock <- commandBlock

  falseBlock <- optionMaybe $ do
    reserved' "else"
    commandBlock

  return (If cond trueBlock (fromMaybe [] falseBlock))


-- Função auxiliar para analisar comandos While
whileCommand :: Parsec String () Comando
whileCommand = do
  reserved' "while"
  cond <-
    try (parens' logicalExpression)
      <|> try (parens' $ Rel <$> relationalExpression)
      <|> try (Rel <$> relationalExpression)

  While cond <$> commandBlock

-- Função auxiliar para analisar blocos de comandos
commandBlock :: Parsec String () [Comando]
commandBlock =
  try (braces' block)
    <|> try (braces' $ return [])
    <|> try (braces' $ many1 command)
    <|> try (braces' $ many command)


-- Função auxiliar para analisar comandos de atribuição
atribCommand :: Parsec String () Comando
atribCommand = do
  var <- identifier'
  reservedOp' "="
  expr <- expression
  semi'
  return (Atrib var expr)

-- Função auxiliar para analisar comandos de leitura (read)
readCommand :: Parsec String () Comando
readCommand = do
  reserved' "read"
  var <- parens' identifier'
  semi'
  return (Leitura var)

-- Função auxiliar para analisar comandos de impressão (print)
printCommand :: Parsec String () Comando
printCommand = do
  reserved' "print"
  expr <- expression
  semi'
  return (Imp expr)

-- Função auxiliar para analisar comandos de retorno (return)
returnCommand :: Parsec String () Comando
returnCommand = do
  reserved' "return"
  mExpr <- optionMaybe (try expression)
  semi'
  return (Ret mExpr)

-- Função para analisar chamadas de funções
functionCall :: Parsec String () Comando
functionCall = do
  (funcName, params) <- functionCall'
  semi'
  return (Proc funcName params)