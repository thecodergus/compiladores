module FunctionsAndParameters where

import Lexer (braces', identifier', parens', reserved', semi', whiteSpace', commaSep')
import Text.Parsec (Parsec, anyChar, char, many, sepBy, try, (<|>))
import Types (Funcao (..), Type (..), Var (..), Id, Bloco, ExprL ((:|:)))
import BlocksAndCommandLists (block)

-- Função principal para analisar a definição de funções
functionDefinition :: Parsec String () Funcao
functionDefinition = do
  retType <- returnType
  whiteSpace'
  funcName <- identifier'
  params <- parens' parameters
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


-- Função auxiliar para analisar a definição de funções
functionHeader :: Parsec String () (Id, [Var])
functionHeader = do
  id <- identifier'
  params <- parens' (commaSep' identifier')
  return (id, map (:#: TInt) params) 


-- Função auxiliar para analisar a definição de funções
parseFunctionsWithParams :: Parsec String () [(Id, [Var], Bloco)]
parseFunctionsWithParams = do
  many $ do
    (funId, params) <- functionHeader
    funBlock <- block
    return (funId, params, funBlock)
