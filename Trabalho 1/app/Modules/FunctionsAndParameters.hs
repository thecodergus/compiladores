module FunctionsAndParameters where

import Lexer (braces', identifier', parens', reserved', semi', whiteSpace')
import Text.Parsec (Parsec, anyChar, char, many, sepBy, try, (<|>))
import Types (Funcao (..), Type (..), Var (..))

-- Função principal para analisar a definição de funções
functionDefinition :: Parsec String () Funcao
functionDefinition = do
  retType <- returnType
  whiteSpace'
  funcName <- identifier'
  params <- parens' parameters
  _ <- braces' (many anyChar) -- Ignora o corpo da função
  return (funcName :->: (params, retType))

-- Função auxiliar para analisar o tipo de retorno de uma função
returnType :: Parsec String () Type
returnType =
  try (reserved' "int" >> return TInt)
    <|> try (reserved' "double" >> return TDouble)
    <|> try (reserved' "string" >> return TString)
    <|> (reserved' "void" >> return TVoid)

-- Função auxiliar para analisar os parâmetros de uma função
parameters :: Parsec String () [Var]
parameters = parameter `sepBy` (char ',' >> whiteSpace')

-- Função auxiliar para analisar um único parâmetro
parameter :: Parsec String () Var
parameter = do
  t <- returnType
  whiteSpace'
  paramName <- identifier'
  return (paramName :#: t)
