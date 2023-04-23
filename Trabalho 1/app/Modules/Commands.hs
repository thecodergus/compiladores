module Commands where

import ArithmeticExpressions (arithmeticExpression)
import Lexer (braces', commaSep', identifier', parens', reserved', reservedOp', semi', stringLiteral', whiteSpace')
import RelationalExpressions (relationalExpression)
import Text.Parsec (Parsec, choice, many, many1, optional, try, (<|>))
import Types (Bloco, Comando (..), Expr, ExprL (..), ExprR)

-- Função principal para analisar comandos
command :: Parsec String () Comando
command =
  choice
    [ ifCommand,
      whileCommand,
      atribCommand,
      readCommand,
      printCommand,
      returnCommand
    ]

-- Função auxiliar para analisar comandos If
ifCommand :: Parsec String () Comando
ifCommand = do
  reserved' "if"
  cond <- parens' logicalExpression
  trueBlock <- braces' block
  reserved' "else"
  falseBlock <- braces' block
  return (If cond trueBlock falseBlock)

-- Função auxiliar para analisar comandos While
whileCommand :: Parsec String () Comando
whileCommand = do
  reserved' "while"
  cond <- parens' logicalExpression
  loopBlock <- braces' block
  return (While cond loopBlock)

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
  expr <- arithmeticExpression
  semi'
  return (Ret expr)

-- Função para analisar expressões lógicas
logicalExpression :: Parsec String () ExprL
logicalExpression = do
  whiteSpace'
  e1 <- try (Rel <$> relationalExpression) <|> parens' logicalExpression
  op <- logicalOperator
  e2 <- try (Rel <$> relationalExpression) <|> parens' logicalExpression
  whiteSpace'
  return (op e1 e2)

-- Função auxiliar para analisar operadores lógicos
logicalOperator :: Parsec String () (ExprL -> ExprL -> ExprL)
logicalOperator =
  try (reservedOp' "&&" >> return (:&:))
    <|> try (reservedOp' "||" >> return (:|:))
    <|> (reservedOp' "!" >> return (\e1 e2 -> Not (e1 :&: e2)))

-- Função para analisar um bloco de comandos
block :: Parsec String () Bloco
block = many1 command
