module LogicalExpressions where

import Lexer (parens', reservedOp', whiteSpace')
import RelationalExpressions (relationalExpression)
import Text.Parsec (Parsec, try, (<|>))
import Types (ExprL (..))

-- Função para analisar expressões lógicas
logicalExpression :: Parsec String () ExprL
logicalExpression = do
  whiteSpace'
  e1 <- try (Rel <$> relationalExpression) <|> parens' logicalExpression
  op <- logicalOperator
  e2 <- try (Rel <$> relationalExpression) <|> parens' logicalExpression
  whiteSpace'
  return (op e1 e2)

-- Função auxiliar para analisar operadores lógicos
logicalOperator :: Parsec String () (ExprL -> ExprL -> ExprL)
logicalOperator =
  try (reservedOp' "&&" >> return (:&:))
    <|> try (reservedOp' "||" >> return (:|:))
    <|> (reservedOp' "!" >> return (\e1 e2 -> Not (e1 :&: e2)))