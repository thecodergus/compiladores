module VariableDeclarationsSpec where

import Test.Hspec ( Spec, describe, it, shouldBe )
import Text.Parsec (parse, Parsec)
import Types ( Type(TVoid, TInt, TDouble, TString), Var((:#:)) )
import VariableDeclarations (variableDeclarations)

parseTest :: Parsec String () a -> String -> Either String a
parseTest parser input =
  case parse parser "" input of
    Left err -> Left (show err)
    Right a -> Right a

spec :: Spec
spec = do
  describe "VariableDeclarations" $ do
    it "parses int variable declaration" $ do
      parseTest variableDeclarations "int a, b, c;" `shouldBe` Right [ "a" :#: TInt,  "b" :#: TInt,  "c" :#: TInt]

    it "parses double variable declaration" $ do
      parseTest variableDeclarations "double x, y;" `shouldBe` Right ["x" :#: TDouble, "y" :#: TDouble]

    it "parses string variable declaration" $ do
      parseTest variableDeclarations "string s1, s2;" `shouldBe` Right ["s1" :#: TString, "s2" :#: TString]

    it "parses void variable declaration" $ do
      parseTest variableDeclarations "void v1;" `shouldBe` Right ["v1" :#: TVoid]

    it "parses single int variable declaration" $ do
      parseTest variableDeclarations "int a;" `shouldBe` Right ["a" :#: TInt]

    it "parses single double variable declaration" $ do
      parseTest variableDeclarations "double x;" `shouldBe` Right ["x" :#: TDouble]

    it "parses single string variable declaration" $ do
      parseTest variableDeclarations "string s1;" `shouldBe` Right ["s1" :#: TString]

    it "parses single void variable declaration" $ do
      parseTest variableDeclarations "void v1;" `shouldBe` Right ["v1" :#: TVoid]
