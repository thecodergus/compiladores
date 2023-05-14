module FunctionsAndParametersSpec where

import Test.Hspec ( Spec, describe, it, shouldBe )
import Text.Parsec (parse, Parsec, ParseError)
import Data.Either (isLeft)
import Types
    ( Type(TString, TVoid, TInt, TDouble), Var((:#:)), Funcao((:->:)) )
import FunctionsAndParameters (functionDefinition)

-- spec :: Spec
-- spec = do
--   describe "FunctionDefinition" $ do
--     it "parses function with no parameters and void return type" $ do
--       parseTest functionDefinition "void myFunction() { }" `shouldBe` Right ("myFunction" :->: ([], TVoid))

--     it "parses function with single int parameter and int return type" $ do
--       parseTest functionDefinition "int myFunction(int a) { }" `shouldBe` Right ("myFunction" :->: (["a" :#: TInt], TInt))

--     it "parses function with multiple parameters and double return type" $ do
--       parseTest functionDefinition "double myFunction(int a, double b, string c) { }" `shouldBe` Right ("myFunction" :->: (["a" :#: TInt, "b" :#: TDouble, "c" :#: TString], TDouble))

--     it "parses function with single string parameter and string return type" $ do
--       parseTest functionDefinition "string myFunction(string s) { }" `shouldBe` Right ("myFunction" :->: (["s" :#: TString], TString))
    
--     it "parses function with multiple parameters and void return type" $ do
--         parseTest functionDefinition "void myFunction(int a, string s, double d) { }" `shouldBe` Right ("myFunction" :->: (["a" :#: TInt, "s" :#: TString, "d" :#: TDouble], TVoid))

--     it "parses function with no parameters and int return type" $ do
--         parseTest functionDefinition "int myFunction() { }" `shouldBe` Right ("myFunction" :->: ([], TInt))

--     it "parses function with void return type and content in the body" $ do
--       parseTest functionDefinition "void myFunction() { int x = 5; }" `shouldBe` Right ("myFunction" :->: ([], TVoid))

--     it "parses function with int return type and content in the body" $ do
--       parseTest functionDefinition "int myFunction(int a) { int x = a * 2; return x; }" `shouldBe` Right ("myFunction" :->: (["a" :#: TInt], TInt))

--     it "parses function with double return type and content in the body" $ do
--       parseTest functionDefinition "double myFunction(int a, double b, string c) { double result = a * b; return result; }" `shouldBe` Right ("myFunction" :->: (["a" :#: TInt, "b" :#: TDouble, "c" :#: TString], TDouble))

--     it "parses function with string return type and content in the body" $ do
--       parseTest functionDefinition "string myFunction(string s) { string result = s ++ \" test\"; return result; }" `shouldBe` Right ("myFunction" :->: (["s" :#: TString], TString))






-- Função auxiliar para testar a análise
parseTest :: Show a => Parsec String () a -> String -> Either ParseError a
parseTest p = parse p ""
