module VariableDeclarations where

import Lexer (commaSep', identifier', reserved', semi', whiteSpace', parens', stringLiteral', const')
import Text.Parsec (Parsec, many, try, (<|>))
import Types ( Var(..), Type(..), Expr (Const, IdVar, Lit, Chamada), TCons (CInt), Id ) 
import Text.Parsec.Token ( GenTokenParser(stringLiteral) )
import Text.Parsec.Combinator (sepBy)
import Text.Parsec.Char (char)

-- Função principal para analisar declarações de variáveis
variableDeclarations :: Parsec String () [Var]
variableDeclarations = do
  t <- varType
  whiteSpace'
  varList <- commaSep' identifier'
  semi'
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

-- Função principal para analisar expressões
expression :: Parsec String () Expr
expression =
  try (parens' expression)
    <|> try (Const <$> const')
    <|> try functionCallExpr
    <|> try arithmeticExpression
    <|> try (IdVar <$> identifier')
    <|> try (Lit <$> stringLiteral')


-- Função auxiliar para analisar chamadas de funções
functionCall' :: Parsec String () (Id, [Expr])
functionCall' = do
  funcName <- identifier'
  params <- parens' $ expression `sepBy` (char ',' >> whiteSpace')
  return (funcName, params)

-- Função auxiliar para analisar chamadas de funções
functionCallExpr :: Parsec String () Expr
functionCallExpr = do
  (funcName, params) <- functionCall'
  return (Chamada funcName params)