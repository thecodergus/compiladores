module ProgramParser where

import BlocksAndCommandLists (block)
import FunctionsAndParameters (parseFunctionsWithParams, functionDefinition)
import Lexer (whiteSpace')
import Text.Parsec (Parsec, eof, many, try, (<|>))
import Types (Bloco, Comando, Funcao, Programa (..), Var)
import VariableDeclarations (variableDeclarations)

-- Função principal para analisar um programa completo
programParser :: Parsec String () Programa
programParser = do
  whiteSpace'
  funDeclarations <- many (try functionDefinition)
  funsWithParams <- parseFunctionsWithParams
  varDeclarations <- try variableDeclarations
  mainBlock <- block
  eof
  return $ Prog funDeclarations funsWithParams varDeclarations mainBlock
