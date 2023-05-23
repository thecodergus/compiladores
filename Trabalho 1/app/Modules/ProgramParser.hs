module ProgramParser where

import FunctionsAndParameters (parseFunctionsWithParamsAndVars, functionDefinition)
import Lexer (whiteSpace', reserved', parens')
import Text.Parsec (Parsec, eof, many, try, (<|>), choice, manyTill, lookAhead, option)
import Types (Bloco, Comando, Funcao, Programa (..), Var, Id)
import Control.Monad (void, unless)
import Test.QuickCheck (Fun(Fun))
import VariableDeclarations (variableDeclarations)
import Data.Either (partitionEithers)
import Data.Maybe (listToMaybe)
import BlocksAndCommands (command, block)
import Data.Foldable (find)

-- Função auxiliar para análise comum
parseCommon :: Parsec String () a -> Parsec String () [a]
parseCommon p = manyTill p (lookAhead eof)

-- Função para analisar e verificar o final do arquivo
parseAndCheckEOF :: Parsec String () a -> Parsec String () a
parseAndCheckEOF p = do
  result <- p
  whiteSpace' -- ignora espaços em branco
  eof -- verifica se chegou ao fim do arquivo
  return result

-- Função principal para analisar um programa completo
programParser :: Parsec String () Programa
programParser = do
  whiteSpace' -- ignora espaços em branco
  let varOrFun =
        -- analisa declarações de variáveis ou funções, e definições de funções
        try (Left . Left <$> variableDeclarations) -- analisa declarações de variáveis
          <|> (Right . Right <$> command) -- analisa comandos
          <|> try (Left . Right <$> functionDefinition) -- analisa declarações de funções
          <|> try (Right . Left <$> parseFunctionsWithParamsAndVars) -- analisa definições de funções
  
  declarations <- parseCommon varOrFun -- analisa declarações de variáveis, definições de funções e comandos

  -- separa declarações de variáveis, definições de funções e comandos
  let (decVarsAndFuncs, rest) = partitionEithers declarations -- separa declarações de variáveis e funções
  let (variableDeclarations, functionDeclarations) = partitionEithers decVarsAndFuncs -- separa declarações de variáveis e funções
  let (funsWithParams, commands) = partitionEithers rest -- separa definições de funções e comandos
  
  unless (null commands) $ fail "Existem comandos fora do escopo de uma função, o que não é permitido na linguagem." -- verifica se existem comandos fora do escopo de uma função

  -- concatena declarações de variáveis e definições de funções
  let variableDeclarations' = concat variableDeclarations -- concatena declarações de variáveis
  let funsWithParams' = concat funsWithParams -- concatena definições de funções
  let mainFunction = find (\(id, _, _, _) -> id == "main") funsWithParams' -- procura pela função main
  
  -- verifica se a função main foi encontrada
  parseAndCheckEOF $ case mainFunction of
    Just (_, _, vars, cmds) -> do
      let mainFunctionCommands = cmds -- pega os comandos da função main
      let mainFunctionVariables = vars -- pega as variáveis da função main
      return $ Prog functionDeclarations variableDeclarations' funsWithParams' mainFunctionVariables mainFunctionCommands -- retorna o programa
    Nothing -> fail "O programa não possui uma função Main definida. A função Main é obrigatória para a execução do programa." -- lança um erro se a função main não for encontrada