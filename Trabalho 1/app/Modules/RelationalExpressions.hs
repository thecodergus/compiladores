module RelationalExpressions where
{-
O módulo RelationalExpressions é usado para analisar expressões relacionais em uma dada linguagem de programação usando a biblioteca Parsec.
Expressões relacionais incluem operações como igual a(==), diferente de(/=), menor que(<), maior que(>), menor ou igual a(<=), e maior ou igual a(>=).
-}

import ArithmeticExpressions (arithmeticExpression)
import Lexer (reservedOp', whiteSpace', parens', reserved', identifier')
import Text.Parsec (Parsec, chainl1, try, (<|>))
import Types (Expr (Const, IdVar, Lit), ExprR (..), Type (TInt), TCons (CInt))

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

