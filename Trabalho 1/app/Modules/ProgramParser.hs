module ProgramParser where
{-
Este módulo fornece a função Programa para analisar programas na linguagem definida pelo professor Cristiano Vasconcellosna disciplina de Compiladores em 2023-1, na UDESC.
-}

import FunctionsAndParameters (funtions, functionDefinition)
import Lexer (whiteSpace', reserved', parens')
import Text.Parsec (Parsec, eof, many, try, (<|>), choice, manyTill, lookAhead, option)
import Types (Bloco, Comando, Funcao, Programa (..), Var, Id)
import Control.Monad (void, unless)
import VariableDeclarations (variableDeclarations)
import Data.Either (partitionEithers)
import Data.Maybe (listToMaybe)
import BlocksAndCommands (command, block)
import Data.Foldable (find)

-- Função principal para analisar um programa completo
programParser :: Parsec String () Programa
programParser = do
    whiteSpace' -- ignora espaços em branco
    
    funs <- funtions -- busca pelo retorno de parseFunctionsWithParamsAndVars

    let (funsDefinitios, funsBlocks) = unzip funs 

    whiteSpace' -- ignora espaços em branco
    eof -- verifica se chegou ao fim do arquivo

    let mainFunction = find (\(id, _, _) -> id == "main") funsBlocks -- procura pela função main
    let (varsMainFunction, mainFunctionCommands) = case mainFunction of -- verifica se a função main foi encontrada
          Just (_, vars, cmds) -> (vars, cmds) -- retorna os comandos da função main
          Nothing -> ([], []) -- retorna uma lista vazia

    -- Retorna o programa contendo apenas as definições de funções.
    -- Note que as listas de FuncaoDefinicao e Var estão vazias.
    return $ Prog funsDefinitios funsBlocks varsMainFunction mainFunctionCommands
