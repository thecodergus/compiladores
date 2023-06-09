module Sintatico.BlocksAndCommands where
{-
Módulo BlocksAndCommands para analisar blocos de código e comandos em uma linguagem de programação usando a biblioteca Parsec.
As funções definidas aqui são usadas para analisar blocos de código que podem aparecer no código fonte, assim como comandos individuais.
Um bloco é uma sequência de comandos, possivelmente incluindo declarações de variáveis (neste projeto decidir separar ambos).
Os comandos que podem ser analisados incluem comandos de atribuição, comandos condicionais if-else, comandos de loop while, comandos de gravação(read) em variavel, comandos de impressão, comandos de retorno e chamadas de função.
-}

import Sintatico.ArithmeticExpressions (arithmeticExpression)
import Sintatico.Lexer (braces', commaSep', identifier', parens', reserved', reservedOp', semi', stringLiteral', whiteSpace')
import Sintatico.LogicalExpressions (logicalExpression, logicalExpression')
import Sintatico.RelationalExpressions (relationalExpression)
import Text.Parsec (Parsec, choice, many, many1, optionMaybe, optional, try, (<|>), sepBy, option)
import Sintatico.Types (Bloco, Comando (..), Expr (Chamada), ExprL (..), ExprR, Id, Var)
import Data.Maybe (fromMaybe)
import Sintatico.VariableDeclarations (expression, functionCall', variableDeclarations)
import Text.Parsec.Char (char)

-- Função pricipal para analisar blocos com declarações de variáveis
block :: Parsec String () ([Var], Bloco)
block = braces' $ do
  vars <- option [] (many (try variableDeclarations)) -- tenta analisar declarações de variáveis
  cmds <- try commandList
  return (concat vars, cmds)

-- Função auxiliar para analisar listas de comandos
commandList :: Parsec String () Bloco
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

-- Função auxiliar para analisar condições
condition :: Parsec String () ExprL
condition =
  try (parens' logicalExpression)
    <|> try (parens' $ Rel <$> relationalExpression)
    <|> try (Rel <$> relationalExpression)

-- Função auxiliar para analisar comandos If
ifCommand :: Parsec String () Comando
ifCommand = do
  reserved' "if"
  cond <- condition

  trueBlock <- commandBlock

  falseBlock <- optionMaybe $ do
    reserved' "else"
    commandBlock

  return (If cond trueBlock (fromMaybe [] falseBlock))


-- Função auxiliar para analisar comandos While
whileCommand :: Parsec String () Comando
whileCommand = do
  reserved' "while"
  cond <- condition

  While cond <$> commandBlock

-- Função auxiliar para analisar blocos de comandos
commandBlock :: Parsec String () [Comando]
commandBlock =
  try (braces' commandList)
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