module Lexer where

import Text.Parsec (Parsec, alphaNum, char, letter, (<|>))
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Token (LanguageDef (..), TokenParser, braces, commaSep, float, identifier, integer, makeTokenParser, parens, reserved, reservedOp, semi, stringLiteral, symbol, whiteSpace)
import Text.ParserCombinators.Parsec.Language (GenLanguageDef (..))
import Types (Expr(IdVar), Id, TCons (CDouble, CInt))

-- Definição da linguagem
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

-- Função auxiliar para analisar tokens
lexer' :: TokenParser ()
lexer' = makeTokenParser languageDef

-- Funções auxiliares para analisar tokens específicos, tokens: identificadores 
identifier' :: Parsec String () Id
identifier' = identifier lexer'

-- Funções auxiliares para analisar tokens específicos, tokens: palavras reservadas
reserved' :: String -> Parsec String () ()
reserved' = reserved lexer'

-- Funções auxiliares para analisar tokens específicos, tokens: operadores reservados
reservedOp' :: String -> Parsec String () ()
reservedOp' = reservedOp lexer'

-- Funções auxiliares para analisar tokens específicos, tokens: parênteses
parens' :: Parsec String () a -> Parsec String () a
parens' = parens lexer'

-- Funções auxiliares para analisar tokens específicos, tokens: chaves
braces' :: Parsec String () a -> Parsec String () a
braces' = braces lexer'

-- Funções auxiliares para analisar tokens específicos, tokens: vírgulas
commaSep' :: Parsec String () a -> Parsec String () [a]
commaSep' = commaSep lexer'

-- Funções auxiliares para analisar tokens específicos, tokens: ponto e vírgula
semi' :: Parsec String () String
semi' = semi lexer'

-- Funções auxiliares para analisar tokens específicos, tokens: inteiros
integer' :: Parsec String () Integer
integer' = integer lexer'

-- Funções auxiliares para analisar tokens específicos, tokens: ponto flutuante
float' :: Parsec String () Double
float' = float lexer'

-- Funções auxiliares para analisar tokens específicos, tokens: strings
stringLiteral' :: Parsec String () String
stringLiteral' = stringLiteral lexer'

-- Funções auxiliares para analisar tokens específicos, tokens: espaços em branco
whiteSpace' :: Parsec String () ()
whiteSpace' = whiteSpace lexer'

-- Funções auxiliares para analisar tokens específicos, tokens: símbolos
symbol' :: String -> Parsec String () String
symbol' = symbol lexer'

-- Funções auxiliares para analisar tokens específicos, tokens: constantes
const' :: Parsec String () TCons
const' = CInt <$> integer'
    <|> CDouble <$> float'