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
import Commands (command)

-- Função principal para analisar um programa completo
programParser :: Parsec String () Programa
programParser = do
      whiteSpace' -- ignora espaços em branco
      let varOrFun = -- analisa declarações de variáveis ou funções, e definições de funções
            try (Left . Left <$> variableDeclarations) -- analisa declarações de variáveis
            <|> try (Left . Right <$> functionDefinition) -- analisa declarações de funções
            <|> try (Right . Left <$> parseFunctionsWithParamsAndVars) -- analisa definições de funções
            
      declarations <- manyTill varOrFun (lookAhead eof) -- analisa declarações até o fim do arquivo

      let (decVarsAndFuncs, rest) = partitionEithers declarations -- separa declarações de variáveis e funções
      let (variableDeclarations, functionDeclarations) = partitionEithers decVarsAndFuncs -- separa declarações de variáveis e funções
      let (funsWithParams, _) = partitionEithers rest -- separa definições de funções e comandos

      whiteSpace' -- ignora espaços em brancow
      eof -- verifica se chegou ao fim do arquivo
      
      return $ Prog functionDeclarations (concat funsWithParams) (concat variableDeclarations) [] -- retorna o programa
