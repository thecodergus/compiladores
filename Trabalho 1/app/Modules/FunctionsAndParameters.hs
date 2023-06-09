module FunctionsAndParameters where
{-
Módulo FunctionsAndParameters para analisar definições de funções e seus parâmetros usando a biblioteca Parsec.
As funções aqui definidas são usadas para analisar definições de funções e declarações de parâmetros que podem aparecer em um código-fonte.
-}

import Lexer (braces', identifier', parens', reserved', semi', whiteSpace', commaSep', type')
import Text.Parsec (Parsec, anyChar, char, many, sepBy, try, (<|>), choice, option, optional)
import Types (Funcao (..), Type (..), Var (..), Id, Bloco, ExprL ((:|:)), Expr, FuncaoBloco)
import VariableDeclarations ( expression )
import BlocksAndCommands (block)

-- Função principal para analisar a definição de funções
functionDefinition :: Parsec String () Funcao
functionDefinition = do
  retType <- type'
  whiteSpace'
  funcName <- identifier'
  params <- parens' parameters
  semi'
  return (funcName :->: (params, retType))


-- Função auxiliar para analisar os parâmetros de uma função
parameters :: Parsec String () [Var]
parameters = parameter `sepBy` (char ',' >> whiteSpace')

-- Função auxiliar para analisar um único parâmetro
parameter :: Parsec String () Var
parameter = do
  t <- type'
  whiteSpace'
  paramName <- identifier'
  return (paramName :#: t)


-- Função auxiliar para analisar a definição de funções
functionHeader :: Parsec String () (Id, [Var], Type)
functionHeader = do
  retType <- type'
  whiteSpace'
  funcName <- identifier'
  params <- parens' parameters
  return (funcName, params, retType)

-- Função auxiliar para analisar a definição de funções
funtions :: Parsec String () [(Funcao, FuncaoBloco)]
funtions = do
  many $ do
    whiteSpace'
    (funId, params, retType) <- try functionHeader
    (vars, funBlock) <- try block
    
    return (funId :->: (params, retType), (funId, params ++ vars, funBlock))