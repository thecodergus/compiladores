module ProgramParser where

import BlocksAndCommandLists (block)
import FunctionsAndParameters (parseFunctionsWithParamsAndVars, functionDefinition)
import Lexer (whiteSpace', reserved', parens')
import Text.Parsec (Parsec, eof, many, try, (<|>), choice, manyTill, lookAhead)
import Types (Bloco, Comando, Funcao, Programa (..), Var, Id)
import Control.Monad (void)
import Test.QuickCheck (Fun(Fun))

-- Função principal para analisar um programa completo
programParser :: Parsec String () Programa
programParser = do
  whiteSpace' -- ignora espaços em branco

  funDeclarations <- try (many (try functionDefinition)) -- analisa declarações de funções
  funsWithParams <- try parseFunctionsWithParamsAndVars -- analisa definições de funções

  let funsWithParams' = map (\(id, params, block, vars) -> (id, params, block)) funsWithParams -- remove as declarações de variáveis das definições de funções
  let varDeclarations = concatMap (\(id, params, block, vars) -> vars) funsWithParams -- cria uma lista de declarações de variáveis
  
  -- mainBlock <- try block

  whiteSpace' -- ignora espaços em branco
  eof -- verifica se chegou ao fim do arquivo
  return $ Prog funDeclarations funsWithParams' varDeclarations [] -- retorna um programa com as declarações de funções, definições de funções, declarações de variáveis e o bloco principal



  -- my_program:: Parsec String () (Funcao, [Var])'