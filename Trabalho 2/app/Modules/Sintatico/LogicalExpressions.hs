module Sintatico.LogicalExpressions where
{-
O módulo LogicalExpressions é usado para analisar expressões lógicas, incluindo operações lógicas como AND(&&), OR(||) e NOT(!).
-}

import Sintatico.Lexer (parens', reservedOp', whiteSpace')
import Sintatico.RelationalExpressions (relationalExpression)
import Text.Parsec (Parsec, try, (<|>))
import Sintatico.Types (ExprL (..))

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
