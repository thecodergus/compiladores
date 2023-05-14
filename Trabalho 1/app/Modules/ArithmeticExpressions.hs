module ArithmeticExpressions where

import Lexer (float', identifier', integer', parens', reservedOp', whiteSpace')
import Text.Parsec (Parsec, chainl1, try, (<|>))
import Types (Expr (..), TCons (..))

-- Função principal para analisar expressões aritméticas
arithmeticExpression :: Parsec String () Expr
arithmeticExpression = whiteSpace' *> term `chainl1` addSubOperator <* whiteSpace'

-- Função auxiliar para analisar termos (multiplicação e divisão)
term :: Parsec String () Expr
term = factor `chainl1` mulDivOperator

-- Função auxiliar para analisar fatores (constantes, variáveis e chamadas de funções)
factor :: Parsec String () Expr
factor =
  try (Const . CInt <$> integer')
    <|> try (Const . CDouble <$> float')
    <|> try (IdVar <$> identifier')
    <|> try (parens' arithmeticExpression)

-- Operadores de adição e subtração
addSubOperator :: Parsec String () (Expr -> Expr -> Expr)
addSubOperator =
  (reservedOp' "+" >> return (:+:))
    <|> (reservedOp' "-" >> return (:-:))

-- Operadores de multiplicação e divisão
mulDivOperator :: Parsec String () (Expr -> Expr -> Expr)
mulDivOperator =
  (reservedOp' "*" >> return (:*:))
    <|> (reservedOp' "/" >> return (:/:))
