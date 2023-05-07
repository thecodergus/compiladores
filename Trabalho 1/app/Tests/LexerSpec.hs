module LexerSpec where

import Lexer (braces', identifier', reserved', reservedOp', semi', whiteSpace')
import Test.Hspec
import Text.Parsec (ParseError, Parsec, parse)

parseTest :: Parsec String () a -> String -> Either ParseError a
parseTest parser input = parse parser "" input

spec :: Spec
spec = do
  describe "Lexer tests" $ do
    it "parses an identifier" $
      parseTest identifier' "someIdentifier" `shouldBe` Right "someIdentifier"

    it "parses reserved word 'if'" $
      parseTest (reserved' "if") "if" `shouldBe` Right ()

    it "parses reserved word 'while'" $
      parseTest (reserved' "while") "while" `shouldBe` Right ()

    it "parses reserved operator '+'" $
      parseTest (reservedOp' "+") "+" `shouldBe` Right ()

    it "parses reserved operator '&&'" $
      parseTest (reservedOp' "&&") "&&" `shouldBe` Right ()

    it "parses a semicolon" $
      parseTest semi' ";" `shouldBe` Right ";"

    it "parses white space" $
      parseTest whiteSpace' "   \t   \n" `shouldBe` Right ()

    it "parses braces" $
      parseTest (braces' $ return "inside") "{inside}" `shouldBe` Right "inside"
