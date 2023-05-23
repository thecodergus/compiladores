module FunctionsAndParameters where

import Lexer (braces', identifier', parens', reserved', semi', whiteSpace', commaSep')
import Text.Parsec (Parsec, anyChar, char, many, sepBy, try, (<|>), choice, option, optional)
import Types (Funcao (..), Type (..), Var (..), Id, Bloco, ExprL ((:|:)), Expr)
import BlocksAndCommandLists (block, block')
import VariableDeclarations ( expression )

-- Função principal para analisar a definição de funções
functionDefinition :: Parsec String () Funcao
functionDefinition = do
  retType <- returnType
  whiteSpace'
  funcName <- identifier'
  params <- parens' parameters
  semi'
  return (funcName :->: (params, retType))

-- Função auxiliar para analisar o tipo de retorno de uma função
returnType :: Parsec String () Type
returnType =
  choice
    [ try (reserved' "int" >> return TInt),
      try ((reserved' "double" <|> reserved' "float") >> return TDouble),
      try (reserved' "string" >> return TString),
      reserved' "void" >> return TVoid
    ]


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
  retType <- returnType
  whiteSpace'
  funcName <- identifier'
  params <- parens' parameters
  return (funcName, params)

-- Função auxiliar para analisar a definição de funções
parseFunctionsWithParamsAndVars :: Parsec String () [(Id, [Var], [Var], Bloco)]
parseFunctionsWithParamsAndVars = do
  many $ do
    whiteSpace'
    (funId, params) <- try functionHeader
    (vars, funBlock) <- try block'
    return (funId, params, vars, funBlock)


-- Função auxiliar para analisar a definição de funções
parseFunctionsWithParamsAndVars' :: Parsec String () (Id, [Var], [Var], Bloco)
parseFunctionsWithParamsAndVars' = do
    whiteSpace'
    (funId, params) <- functionHeader
    (vars, funBlock) <- block'
    return (funId, params, vars, funBlock)
