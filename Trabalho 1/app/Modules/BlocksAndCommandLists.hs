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

  let varOrCmd = try (Left <$> variableDeclarations) <|> (Right <$> command)

  declsAndCmds <- many varOrCmd

  let (varDecls, cmds) = partitionEithers declsAndCmds
  
  return (concat varDecls, cmds)


-- Função auxiliar para analisar blocos com declarações de variáveis
block'' :: Parsec String () Bloco
block'' = braces' $ many (whiteSpace' *> command <* whiteSpace')

-- Função auxiliar para analisar listas de comandos
commandList :: Parsec String () [Comando]
commandList = many (whiteSpace' *> command <* whiteSpace')
