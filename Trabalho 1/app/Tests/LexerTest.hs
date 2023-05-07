module LexerTest where

import Lexer
import Text.Parsec (parseTest)

testIdentifier :: IO ()
testIdentifier = do
  putStrLn "Teste Identificador:"
  parseTest identifier' "variavel"
  parseTest identifier' "_variavel2"

testReserved :: IO ()
testReserved = do
  putStrLn "Teste Palavras Reservadas:"
  parseTest (reserved' "int") "int"
  parseTest (reserved' "double") "double"

testReservedOp :: IO ()
testReservedOp = do
  putStrLn "Teste Operadores Reservados:"
  parseTest (reservedOp' "+") "+"
  parseTest (reservedOp' "==") "=="

testSemi :: IO ()
testSemi = do
  putStrLn "Teste Ponto e vírgula:"
  parseTest semi' ";"

main :: IO ()
main = do
  testIdentifier
  testReserved
  testReservedOp
  testSemi
