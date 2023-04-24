module VariableDeclarations where

import Lexer (commaSep', identifier', reserved', semi', whiteSpace')
import Text.Parsec (Parsec, many, try, (<|>))
import Types (Type (..), Var (..))

-- Função principal para analisar declarações de variáveis
variableDeclarations :: Parsec String () [Var]
variableDeclarations = do
  t <- varType
  semi'
  whiteSpace'
  varList <- commaSep' identifier'
  return (createVariables t varList)

-- Função auxiliar para criar uma lista de variáveis a partir de um tipo e uma lista de identificadores
createVariables :: Type -> [String] -> [Var]
createVariables t ids = [i :#: t | i <- ids]

-- Função auxiliar para analisar o tipo de uma variável
varType :: Parsec String () Type
varType =
  try (reserved' "int" >> return TInt)
    <|> try (reserved' "double" >> return TDouble)
    <|> try (reserved' "string" >> return TString)
    <|> (reserved' "void" >> return TVoid)
