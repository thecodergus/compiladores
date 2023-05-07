module Commands where

import ArithmeticExpressions (arithmeticExpression)
import Lexer (braces', commaSep', identifier', parens', reserved', reservedOp', semi', stringLiteral', whiteSpace')
import LogicalExpressions (logicalExpression, logicalExpression')
import RelationalExpressions (relationalExpression)
import Text.Parsec (Parsec, choice, many, many1, optionMaybe, optional, try, (<|>))
import Types (Bloco, Comando (..), Expr, ExprL (..), ExprR)
import Data.Maybe (fromMaybe)

-- Função principal para analisar comandos
command :: Parsec String () Comando
command =
  choice
    [ ifCommand,
      whileCommand,
      atribCommand,
      readCommand,
      printCommand,
      returnCommand,
      procCommand
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
  expr <- arithmeticExpression
  semi'
  return (Atrib var expr)

-- Função auxiliar para analisar comandos de leitura (read)
readCommand :: Parsec String () Comando
readCommand = do
  reserved' "read"
  var <- identifier'
  semi'
  return (Leitura var)

-- Função auxiliar para analisar comandos de impressão (print)
printCommand :: Parsec String () Comando
printCommand = do
  reserved' "print"
  expr <- arithmeticExpression
  semi'
  return (Imp expr)

-- Função auxiliar para analisar comandos de retorno (return)
returnCommand :: Parsec String () Comando
returnCommand = do
  reserved' "return"
  mExpr <- optionMaybe (try arithmeticExpression)
  semi'
  return (Ret mExpr)

-- Função para analisar um bloco de comandos
block :: Parsec String () Bloco
block = many1 command

-- Função auxiliar para analisar comandos de chamada de procedimento (Proc)
procCommand :: Parsec String () Comando
procCommand = do
  procName <- identifier'
  args <- parens' (commaSep' arithmeticExpression)
  semi'
  return (Proc procName args)
