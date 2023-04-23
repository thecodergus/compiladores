-- Módulo Lexer.hs: Implementa os parsers para tokens básicos e palavras reservadas.
module Lexer where

import Text.Parsec (alphaNum, char, letter, oneOf, (<|>))
import Text.Parsec.Language (LanguageDef, emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token
  ( GenLanguageDef
      ( LanguageDef,
        caseSensitive,
        commentEnd,
        commentLine,
        commentStart,
        identLetter,
        identStart,
        nestedComments,
        opLetter,
        opStart,
        reservedNames,
        reservedOpNames
      ),
    GenTokenParser (reserved),
    LanguageDef,
    TokenParser,
    makeTokenParser,
  )
import Types (Type (..))

-- Define a linguagem de programação específica.
languageDef :: LanguageDef ()
languageDef =
  LanguageDef
    { commentStart = "/*",
      commentEnd = "*/",
      commentLine = "#",
      nestedComments = True,
      identStart = letter <|> char '_',
      identLetter = alphaNum <|> char '_',
      opStart = oneOf ":!#$%&*+./<=>?@\\^|-~",
      opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~",
      reservedOpNames = ["+", "-", "*", "/", "==", "/=", "<", ">", "<=", ">=", "&&", "||", "!"],
      reservedNames = ["int", "double", "string", "void", "return", "if", "else", "while", "print", "read"],
      caseSensitive = True
    }

-- Cria um lexer com base na definição da linguagem.
lexer :: TokenParser ()
lexer = makeTokenParser languageDef

-- Parser para os tipos de dados suportados pela linguagem.
typeParser :: Parser Type
typeParser =
  fmap (const TInt) (reserved lexer "int")
    <|> fmap (const TDouble) (reserved lexer "double")
    <|> fmap (const TString) (reserved lexer "string")
    <|> fmap (const TVoid) (reserved lexer "void")
