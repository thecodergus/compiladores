module ProgramParser where

import BlocksAndCommandLists (block)
import FunctionsAndParameters (parseFunctionsWithParamsAndVars, functionDefinition)
import Lexer (whiteSpace', reserved', parens')
import Text.Parsec (Parsec, eof, many, try, (<|>), choice, manyTill, lookAhead, option)
import Types (Bloco, Comando, Funcao, Programa (..), Var, Id)
import Control.Monad (void)
import Test.QuickCheck (Fun(Fun))
import VariableDeclarations (variableDeclarations)

-- Função principal para analisar um programa completo
-- programParser :: Parsec String () Programa
-- programParser = do
--   whiteSpace' -- ignora espaços em branco

--   variableDeclarations1 <- option [] (try variableDeclarations) -- analisa declarações de variáveis antes das declarações de funções
--   funDeclarations <- option [] (try (many (try functionDefinition))) -- analisa declarações de funções
--   variableDeclarations2 <- option [] (try variableDeclarations) -- analisa declarações de variáveis após as declarações de funções
--   funsWithParams <- option [] (try parseFunctionsWithParamsAndVars) -- analisa definições de funções
--   mainBlock <- option [] (try block) -- analisa o bloco principal
--   whiteSpace' -- ignora espaços em branco
--   eof -- verifica se chegou ao fim do arquivo

--   let variableDeclarations = variableDeclarations1 ++ variableDeclarations2 -- concatena as declarações de variáveis

--   return $ Prog funDeclarations funsWithParams variableDeclarations mainBlock -- retorna um programa com as declarações de funções, definições de funções, declarações de variáveis e o bloco principal

