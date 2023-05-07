module ProgramParserSpec where
import Text.Parsec (Parsec, ParseError, parse)

import Test.Hspec (Expectation, Spec, describe, it, shouldBe, hspec)
import Types
    ( Type(TDouble, TInt, TVoid),
      Var((:#:)),
      Programa(Prog),
      Funcao((:->:)),
      Expr(IdVar, (:+:), (:*:)), Comando (Ret) )
import ProgramParser (programParser)




parseTest :: Parsec String () a -> String -> Either ParseError a
parseTest p = parse p ""

spec :: Spec
spec = do
  describe "programParser" $ do
    it "parses an empty program" $ do
      parseTest programParser "void main() {}" `shouldBe` Right (Prog ["main" :->: ([],TVoid)] [] [] [])

    it "parses a program with a single function declaration" $ do
      let prog = "int sum(int a, int b);\nvoid main() {}"
      parseTest programParser prog `shouldBe` Right (Prog ["sum" :->: (["a" :#: TInt, "b" :#: TInt], TInt)] [] [] [])

    it "parses a program with a function definition and a variable declaration" $ do
      let prog = "int sum(int a, int b) { return a + b; }\nint x;\nvoid main() {}"
      let fun = ("sum", ["a" :#: TInt, "b" :#: TInt], [Ret (Just ((:+:) (IdVar "a") (IdVar "b")))])
      parseTest programParser prog `shouldBe` Right (Prog ["sum" :->: (["a" :#: TInt, "b" :#: TInt], TInt)] [fun] ["x" :#: TInt] [])

    it "parses a program with multiple function declarations and definitions" $ do
      let prog = "int sum(int a, int b);\ndouble product(double x, double y);\nint sum(int a, int b) { return a + b; }\ndouble product(double x, double y) { return x * y; }\nvoid main() {}"
      let funSum = ("sum", ["a" :#: TInt, "b" :#: TInt], [Ret (Just ((:+:) (IdVar "a") (IdVar "b")))])
      let funProduct = ("product", ["x" :#: TDouble, "y" :#: TDouble], [Ret (Just ((:*:) (IdVar "x") (IdVar "y")))])
      parseTest programParser prog `shouldBe` Right (Prog ["sum" :->: (["a" :#: TInt, "b" :#: TInt], TInt), "product" :->: (["x" :#: TDouble, "y" :#: TDouble], TDouble)] [funSum, funProduct] [] [])