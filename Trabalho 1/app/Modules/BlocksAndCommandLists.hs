module BlocksAndCommandLists where

import Commands (command)
import Lexer (braces', whiteSpace')
import Text.Parsec (Parsec, many, try, (<|>), option)
import Types (Bloco, Comando, Var)
import VariableDeclarations (variableDeclarations)

-- Função principal para analisar blocos
block :: Parsec String () Bloco
block = braces' commandList

-- Função auxiliar para analisar blocos com declarações de variáveis
block' :: Parsec String () ([Var], Bloco)
block' = braces' $ do
  vars <- option [] (try variableDeclarations) -- tenta analisar declarações de variáveis
  cmds <- try commandList
  return (vars, cmds)

block'' = braces' $ many (whiteSpace' *> command <* whiteSpace')
block'' :: Parsec String () Bloco

-- Função auxiliar para analisar listas de comandos
commandList :: Parsec String () [Comando]
commandList = many (whiteSpace' *> command <* whiteSpace')
