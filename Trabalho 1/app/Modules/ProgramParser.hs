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
import Data.Maybe (listToMaybe)

-- Função principal para analisar um programa completo
programParser :: Parsec String () Programa
programParser = do
  whiteSpace' -- ignora espaços em branco
  let varOrFun =
        try (Left <$> functionDefinition)
          <|> try (Right . Left <$> variableDeclarations)
          <|> try (Right . Right . Left <$> parseFunctionsWithParamsAndVars)
          <|> try (Right . Right . Right <$> block)

  declarations <- manyTill varOrFun (lookAhead eof)

  let (funDeclarations, rest) = partitionEithers declarations
  let (variableDeclarations, rest') = partitionEithers rest
  let (funsWithParams, mainBlocks) = partitionEithers rest'

  let mainBlock = case mainBlocks of
        [] -> []
        (b : _) -> b

  whiteSpace' -- ignora espaços em branco
  eof -- verifica se chegou ao fim do arquivo
  return $ Prog funDeclarations (concat funsWithParams) (concat variableDeclarations) mainBlock
