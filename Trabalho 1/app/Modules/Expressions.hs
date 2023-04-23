-- Módulo Expressions.hs: Implementa os parsers para expressões.
module Expressions where

import Control.Monad (liftM)
import Control.Monad.Identity (Identity)
import Lexer (lexer)
import Text.Parsec (choice, try, (<?>))
import Text.Parsec.Expr
  ( Assoc (AssocLeft),
    Operator (Infix, Prefix),
    buildExpressionParser,
  )
import Text.Parsec.String (Parser)
import Text.Parsec.Token
  ( GenTokenParser (float, identifier, integer, parens, reservedOp),
  )
import Types
  ( Expr (Const, IdVar, Neg, (:*:), (:+:), (:-:), (:/:)),
    TCons (CDouble, CInt),
  )

-- Parser para expressões aritméticas.
expr :: Parser Expr
expr = buildExpressionParser table factor <?> "expression"

-- Tabela de operadores.
table :: [[Operator String () Identity Expr]]
table =
  [ [prefix "-" Neg, prefix "+" id],
    [binary "*" (:*:) AssocLeft, binary "/" (:/:) AssocLeft],
    [binary "+" (:+:) AssocLeft, binary "-" (:-:) AssocLeft]
  ]

-- Funções auxiliares para a tabela de operadores.
binary :: String -> (Expr -> Expr -> Expr) -> Assoc -> Operator String () Identity Expr
binary name fun = Infix (do reservedOp lexer name; return fun)

prefix :: String -> (Expr -> Expr) -> Operator String () Identity Expr
prefix name fun = Prefix (do reservedOp lexer name; return fun)

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