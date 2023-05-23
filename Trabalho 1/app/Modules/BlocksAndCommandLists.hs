module BlocksAndCommandLists where

import Commands (command)
import Lexer (braces', whiteSpace')
import Text.Parsec (Parsec, many, try, (<|>), option, manyTill, lookAhead, eof)
import Types (Bloco, Comando, Var)
import VariableDeclarations (variableDeclarations)
import Data.Either (partitionEithers)

-- Função principal para analisar blocos
block :: Parsec String () Bloco
block = braces' commandList

-- Função auxiliar para analisar blocos com declarações de variáveis
block' :: Parsec String () ([Var], Bloco)
block' = braces' $ do
  vars <- option [] (try variableDeclarations) -- tenta analisar declarações de variáveis
  cmds <- try commandList
  return (vars, cmds)

-- Função auxiliar para analisar blocos com declarações de variáveis
block'' :: Parsec String () Bloco
block'' = braces' $ many (whiteSpace' *> command <* whiteSpace')

-- Função auxiliar para analisar bloco com declarações de variaveis e comandos de forma mais maleavel
block''' :: Parsec String () ([Var], Bloco)
block''' = do
  braces' whiteSpace'
  let varOrCmds = 
        try (Left . Left <$> variableDeclarations)
        <|> try (Right . Right <$> command)

  declarations <- manyTill varOrCmds (lookAhead eof)
  let (um, dois) = partitionEithers declarations
  let (um', _) = partitionEithers um
  let (dois', _) = partitionEithers dois

  return (concat um', concat dois')

-- Função auxiliar para analisar listas de comandos
commandList :: Parsec String () [Comando]
commandList = many (whiteSpace' *> command <* whiteSpace')
