module Statements where

import Lexer (lexer, typeParser)
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec (Parser, (<?>))
import Text.ParserCombinators.Parsec.Token
  ( GenTokenParser (identifier, reservedOp),
  )
import Types (Type (TDouble, TInt, TString, TVoid), Var (..))

-- Funções auxiliares
identifier' :: Parser String
identifier' = identifier lexer

reservedOp' :: String -> Parser ()
reservedOp' = reservedOp lexer

-- Parser para declarações de variáveis.
var :: Parser Var
var =
  do
    vName <- identifier'
    reservedOp' ":"
    vType <- typeParser
    return (vName :#: vType)
    <?> "var"
