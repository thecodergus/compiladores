module ProgramParser where

import BlocksAndCommandLists (block)
import FunctionsAndParameters (parseFunctionsWithParams, functionDefinition)
import Lexer (whiteSpace', reserved', parens')
import Text.Parsec (Parsec, eof, many, try, (<|>), choice, manyTill, lookAhead)
import Types (Bloco, Comando, Funcao, Programa (..), Var, Id)
import VariableDeclarations (variableDeclarations)
import Control.Monad (void)

-- Função principal para analisar um programa completo
programParser :: Parsec String () Programa
programParser = do
  whiteSpace'
  funDeclarations <- many (try functionDefinition)
  funsWithParams <- parseFunctionsWithParams
  varDeclarations <- variableDeclarations
  mainBlock <- block
  eof
  return $ Prog funDeclarations funsWithParams varDeclarations mainBlock