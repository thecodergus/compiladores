-- Módulo Expressions.hs: Implementa os parsers para expressões.
module Expressions where

import Control.Monad (liftM, liftM2)
import Control.Monad.Identity (Identity)
import Lexer (languageDef, lexer)
import Text.Parsec (choice, try, (<?>), (<|>))
import Text.Parsec.Expr
  ( Assoc (AssocLeft),
    Operator (Infix, Prefix),
    buildExpressionParser,
  )
import Text.Parsec.String (Parser)
import Text.Parsec.Token
  ( GenTokenParser (float, identifier, integer, parens, reservedOp),
    TokenParser,
    makeTokenParser,
  )
import Types
  ( Expr (..),
    ExprL (..),
    ExprR (..),
    TCons (CDouble, CInt),
  )

-- Parser para expressões aritméticas.
expr :: Parser Expr
expr = buildExpressionParser table factor <?> "expression"

-- Tabela de operadores.
table :: [[Operator String () Identity Expr]]
table =
  [ [prefix' "-" Neg, prefix' "+" id],
    [binary' "*" (:*:) AssocLeft, binary' "/" (:/:) AssocLeft],
    [binary' "+" (:+:) AssocLeft, binary' "-" (:-:) AssocLeft]
  ]

-- Funções auxiliares para a tabela de operadores.
binary' :: String -> (a -> a -> a) -> Assoc -> Operator String () Identity a
binary' name fun = Infix (do reservedOp lexer name; return fun)

prefix' :: String -> (a -> a) -> Operator String () Identity a
prefix' name fun = Prefix (do reservedOp lexer name; return fun)

lexer' :: TokenParser ()
lexer' = makeTokenParser languageDef

reservedOp' :: String -> Parser ()
reservedOp' = reservedOp lexer

parens' :: Parser a -> Parser a
parens' = parens lexer

-- Parser para fatores em uma expressão.
factor :: Parser Expr
factor =
  choice
    [ try (parens lexer expr),
      fmap IdVar (identifier lexer),
      fmap (Const . CInt . fromInteger) (integer lexer),
      fmap (Const . CDouble) (float lexer)
    ]
    <?> "factor"

-- Parser para expressões relacionais.
exprR :: Parser ExprR
exprR =
  try (liftM2 (:==:) expr (reservedOp' "==" >> expr))
    <|> try (liftM2 (:/=:) expr (reservedOp' "/=" >> expr))
    <|> try (liftM2 (:<:) expr (reservedOp' "<" >> expr))
    <|> try (liftM2 (:>:) expr (reservedOp' ">" >> expr))
    <|> try (liftM2 (:<=:) expr (reservedOp' "<=" >> expr))
    <|> liftM2 (:>=:) expr (reservedOp' ">=" >> expr)
    <?> "exprR"

exprL :: Parser ExprL
exprL = buildExpressionParser table term <?> "exprL"
  where
    table =
      [ [Prefix (reservedOp' "!" >> return Not)],
        [Infix (reservedOp' "&&" >> return (:&:)) AssocLeft],
        [Infix (reservedOp' "||" >> return (:|:)) AssocLeft]
      ]
    term = parens' exprL <|> fmap Rel exprR <?> "term"
