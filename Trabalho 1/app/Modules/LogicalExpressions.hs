module LogicalExpressions where

import Lexer (parens', reservedOp', whiteSpace')
import RelationalExpressions (relationalExpression)
import Text.Parsec (Parsec, try, (<|>))
import Types (ExprL (..))

-- Função para analisar expressões lógicas
logicalExpression :: Parsec String () ExprL
logicalExpression = whiteSpace' *> (try notExpression <|> logicalExpression') <* whiteSpace'

-- Função auxiliar para analisar expressões lógicas sem NOT
logicalExpression' :: Parsec String () ExprL
logicalExpression' = do
  e1 <- try (parens' logicalExpression) <|> try (parens' $ Rel <$> relationalExpression) <|> try (Rel <$> relationalExpression)
  op <- logicalOperator
  e2 <- try (parens' logicalExpression) <|> try (parens' $ Rel <$> relationalExpression) <|> try (Rel <$> relationalExpression)
  whiteSpace'
  return (op e1 e2)


-- Função auxiliar para analisar a expressão NOT
notExpression :: Parsec String () ExprL
notExpression = do
  reservedOp' "!"
  e <- try (parens' logicalExpression) <|> try (parens' $ Rel <$> relationalExpression) <|> try (Rel <$> relationalExpression)
  return (Not e)


-- Função auxiliar para analisar operadores lógicos
logicalOperator :: Parsec String () (ExprL -> ExprL -> ExprL)
logicalOperator =
  try (reservedOp' "&&" >> return (:&:))
    <|> try (reservedOp' "||" >> return (:|:))
    <|> (reservedOp' "!" >> return (\e1 e2 -> Not (e1 :&: e2)))
