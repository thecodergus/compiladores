module ProgramParser where

import BlocksAndCommandLists (block)
import FunctionsAndParameters (parseFunctionsWithParams, functionDefinition)
import Lexer (whiteSpace', reserved', parens')
import Text.Parsec (Parsec, eof, many, try, (<|>), choice, manyTill, lookAhead)
import Types (Bloco, Comando, Funcao, Programa (..), Var, Id)
import VariableDeclarations (variableDeclarations)
import Control.Monad (void)
import Test.QuickCheck (Fun(Fun))

-- Função principal para analisar um programa completo
programParser :: Parsec String () Programa
programParser = do
  whiteSpace'
  funDeclarations <- try (many (try functionDefinition))
  funsWithParams <- try parseFunctionsWithParams
  -- varDeclarations <- variableDeclarations
  -- mainBlock <- block
  eof
  return $ Prog funDeclarations funsWithParams [] []



  -- my_program:: Parsec String () (Funcao, [Var])'