module BlocksAndCommandLists where

import Commands (command)
import Lexer (braces', whiteSpace')
import Text.Parsec (Parsec, many, try, (<|>))
import Types (Bloco, Comando)

-- Função principal para analisar blocos
block :: Parsec String () Bloco
block = braces' commandList

-- Função auxiliar para analisar listas de comandos
commandList :: Parsec String () [Comando]
commandList = many (whiteSpace' *> command <* whiteSpace')
