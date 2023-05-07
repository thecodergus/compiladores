module LexerSpec where

import Lexer (braces', identifier', reserved', reservedOp', semi', whiteSpace', parens')
import Test.Hspec ( Spec, describe, it, shouldBe )
import Text.Parsec (ParseError, Parsec, parse, string)

parseTest :: Parsec String () a -> String -> Either ParseError a
parseTest parser input = parse parser "" input

spec :: Spec
spec = do
  describe "Lexer" $ do
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
      parseTest (braces' $ string "inside") "{inside}" `shouldBe` Right "inside"
    
    it "parses braces with multiple characters inside" $
      parseTest (braces' $ string "content") "{content}" `shouldBe` Right "content"

    it "parses braces with nested braces" $
      parseTest (braces' $ string "outer" *> braces' (string "inner")) "{outer{inner}}" `shouldBe` Right "inner"

    it "parses parentheses" $
      parseTest (parens' $ string "inside") "(inside)" `shouldBe` Right "inside"

