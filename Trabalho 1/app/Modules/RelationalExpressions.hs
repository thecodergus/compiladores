module RelationalExpressions where

import ArithmeticExpressions (arithmeticExpression)
import Lexer (reservedOp', whiteSpace')
import Text.Parsec (Parsec, chainl1, try, (<|>))
import Types (Expr, ExprR (..))

-- Função principal para analisar expressões relacionais
relationalExpression :: Parsec String () ExprR
relationalExpression = do
  whiteSpace'
  e1 <- arithmeticExpression
  op <- relationalOperator
  e2 <- arithmeticExpression
  whiteSpace'
  return (op e1 e2)

-- Operadores relacionais
relationalOperator :: Parsec String () (Expr -> Expr -> ExprR)
relationalOperator =
  try (reservedOp' "==" >> return (:==:))
    <|> try (reservedOp' "/=" >> return (:/=:))
    <|> try (reservedOp' "<" >> return (:<:))
    <|> try (reservedOp' ">" >> return (:>:))
    <|> try (reservedOp' "<=" >> return (:<=:))
    <|> (reservedOp' ">=" >> return (:>=:))
