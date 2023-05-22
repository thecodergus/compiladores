module ProgramParser where

import BlocksAndCommandLists (block)
import FunctionsAndParameters (parseFunctionsWithParamsAndVars, functionDefinition)
import Lexer (whiteSpace', reserved', parens')
import Text.Parsec (Parsec, eof, many, try, (<|>), choice, manyTill, lookAhead, option)
import Types (Bloco, Comando, Funcao, Programa (..), Var, Id)
import Control.Monad (void, unless)
import Test.QuickCheck (Fun(Fun))
import VariableDeclarations (variableDeclarations)
import Data.Either (partitionEithers)
import Data.Maybe (listToMaybe)
import Commands (command)
import Data.Foldable (find)

-- Função principal para analisar um programa completo
programParser :: Parsec String () Programa
programParser = do
      whiteSpace' -- ignora espaços em branco
      let varOrFun = -- analisa declarações de variáveis ou funções, e definições de funções
            try (Left . Left <$> variableDeclarations) -- analisa declarações de variáveis
            <|> (Right . Right <$> command) -- analisa comandos
            <|> try (Left . Right <$> functionDefinition) -- analisa declarações de funções
            <|> try (Right . Left <$> parseFunctionsWithParamsAndVars) -- analisa definições de funções

      declarations <- manyTill varOrFun (lookAhead eof) -- analisa declarações até o fim do arquivo

      -- separa declarações de variáveis, definições de funções e comandos
      let (decVarsAndFuncs, rest) = partitionEithers declarations -- separa declarações de variáveis e funções
      let (variableDeclarations, functionDeclarations) = partitionEithers decVarsAndFuncs -- separa declarações de variáveis e funções
      let (funsWithParams, commands) = partitionEithers rest -- separa definições de funções e comandos
      
      unless (null commands) $ fail "Existem comandos fora do escopo de uma função, o que não é permitido." -- verifica se existem comandos fora do escopo de uma função


      let variableDeclarations' = concat variableDeclarations -- concatena declarações de variáveis
      let funsWithParams' = concat funsWithParams -- concatena definições de funções

      whiteSpace' -- ignora espaços em brancow
      eof -- verifica se chegou ao fim do arquivo

      let mainFunction = find (\(id, _, _, _) -> id == "main") funsWithParams' -- procura pela função main

      case mainFunction of
            Just (_, _, vars, cmds) -> do
                  
                  let mainFunctionCommands = cmds -- pega os comandos da função main
                  let mainFunctionVariables = vars -- pega as variáveis da função main

                  return $ Prog functionDeclarations variableDeclarations' funsWithParams' mainFunctionVariables mainFunctionCommands -- retorna o programa

            Nothing -> fail "O programa precisa ter uma função principal Main para ser o bloco principal da função programa." -- lança um erro se a função main não for encontrada