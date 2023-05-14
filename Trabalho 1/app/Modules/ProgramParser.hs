module ProgramParser where

import BlocksAndCommandLists (block)
import FunctionsAndParameters (parseFunctionsWithParamsAndVars, functionDefinition)
import Lexer (whiteSpace', reserved', parens')
import Text.Parsec (Parsec, eof, many, try, (<|>), choice, manyTill, lookAhead, option)
import Types (Bloco, Comando, Funcao, Programa (..), Var, Id)
import Control.Monad (void)
import Test.QuickCheck (Fun(Fun))
import VariableDeclarations (variableDeclarations)
import Data.Either (partitionEithers)

-- Função principal para analisar um programa completo
programParser :: Parsec String () Programa
programParser = do
  whiteSpace' -- ignora espaços em branco
  
  let varOrFun = try (Left <$> functionDefinition) <|> try (Right <$> variableDeclarations) -- tenta analisar uma definição de função ou uma declaração de variável
  declarations <- manyTill varOrFun (lookAhead (try (void parseFunctionsWithParamsAndVars) <|> void eof)) -- analisa definições de funções e declarações de variáveis
  
  let (funDeclarations, variableDeclarations) = partitionEithers declarations -- separa as declarações de funções das declarações de variáveis
  
  funsWithParams <- option [] (try parseFunctionsWithParamsAndVars) -- analisa definições de funções
  
  mainBlock <- option [] (try block) -- analisa o bloco principal
  
  whiteSpace' -- ignora espaços em branco
  eof -- verifica se chegou ao fim do arquivo

  return $ Prog funDeclarations funsWithParams (concat variableDeclarations) mainBlock -- retorna um programa com as declarações de funções, definições de funções, declarações de variáveis e o bloco principal
