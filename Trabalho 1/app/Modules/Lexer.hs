module Lexer where

import Text.Parsec (Parsec, alphaNum, char, letter, (<|>))
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Token (LanguageDef (..), TokenParser, braces, commaSep, float, identifier, integer, makeTokenParser, parens, reserved, reservedOp, semi, stringLiteral, whiteSpace, symbol)
import Text.ParserCombinators.Parsec.Language (GenLanguageDef (..))
import Types (Id)

lexer' :: TokenParser ()
lexer' = makeTokenParser languageDef

languageDef :: LanguageDef ()
languageDef =
  emptyDef
    { commentStart = "/*",
      commentEnd = "*/",
      commentLine = "//",
      identStart = letter <|> char '_',
      identLetter = alphaNum <|> char '_',
      reservedNames = ["int", "double", "string", "void", "if", "else", "while", "return", "print", "read"],
      reservedOpNames = ["+", "-", "*", "/", "=", "<", ">", "<=", ">=", "==", "/=", "&&", "||", "!"],
      caseSensitive = True
    }

identifier' :: Parsec String () Id
identifier' = identifier lexer'

reserved' :: String -> Parsec String () ()
reserved' = reserved lexer'

reservedOp' :: String -> Parsec String () ()
reservedOp' = reservedOp lexer'

parens' :: Parsec String () a -> Parsec String () a
parens' = parens lexer'

braces' :: Parsec String () a -> Parsec String () a
braces' = braces lexer'

commaSep' :: Parsec String () a -> Parsec String () [a]
commaSep' = commaSep lexer'

semi' :: Parsec String () String
semi' = semi lexer'

integer' :: Parsec String () Integer
integer' = integer lexer'

float' :: Parsec String () Double
float' = float lexer'

stringLiteral' :: Parsec String () String
stringLiteral' = stringLiteral lexer'

whiteSpace' :: Parsec String () ()
whiteSpace' = whiteSpace lexer'

symbol' :: String -> Parsec String ()
symbol' = symbol lexer'