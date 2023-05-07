module ProgramParser where

import BlocksAndCommandLists (block)
import FunctionsAndParameters (functionDefinition)
import Lexer (whiteSpace')
import Text.Parsec (Parsec, eof, many, try, (<|>))
import Types (Bloco, Comando, Funcao, Programa (..), Var)
import VariableDeclarations (variableDeclarations)

-- Função principal para analisar um programa completo
programParser :: Parsec String () Programa
programParser = do
  whiteSpace'
  funDeclarations <- many functionDefinition
  varDeclarations <- variableDeclarations
  mainBlock <- block
  eof
  return $ Prog funDeclarations [] [] mainBlock
  -- return $ Prog funDeclarations [] varDeclarations mainBlock
