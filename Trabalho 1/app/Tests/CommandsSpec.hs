module CommandsSpec where

import Test.Hspec
import Text.Parsec (Parsec, parse)
import Commands (command, Command(..))
import Types (Id(..), Expr(..), TCons(..), ExprL(..), ExprR(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "command" $ do
    it "parses assignment command" $ do
      parseTest command "a = 10;" `shouldBe` Right (Assign (IdVar "a") (Const (CInt 10)))

    it "parses if command" $ do
      parseTest command "if (a > b) a = 20; else a = 10;" `shouldBe`
        Right (If (Rel (IdVar "a" :>: IdVar "b")) (Assign (IdVar "a") (Const (CInt 20))) (Assign (IdVar "a") (Const (CInt 10))))

    it "parses while command" $ do
      parseTest command "while (a < 10) a = a + 1;" `shouldBe`
        Right (While (Rel (IdVar "a" :<: Const (CInt 10))) (Assign (IdVar "a") (IdVar "a" :+: Const (CInt 1))))

    it "parses print command" $ do
      parseTest command "print(a);" `shouldBe` Right (Print (IdVar "a"))

    it "parses read command" $ do
      parseTest command "read(a);" `shouldBe` Right (Read (IdVar "a"))

    it "parses atribCommand" $ do
      parseTest command "a = b + 1;" `shouldBe`
        Right (Assign (IdVar "a") (IdVar "b" :+: Const (CInt 1)))

    it "parses returnCommand" $ do
      parseTest command "return a + 1;" `shouldBe`
        Right (Return (IdVar "a" :+: Const (CInt 1)))

    it "parses block" $ do
      parseTest command "{ a = b + 1; b = a - 1; }" `shouldBe`
        Right (Block [Assign (IdVar "a") (IdVar "b" :+: Const (CInt 1)),
                      Assign (IdVar "b") (IdVar "a" :-: Const (CInt 1))])

    it "parses procCommand" $ do
      parseTest command "myFunction(a, b, c);" `shouldBe`
        Right (ProcCall "myFunction" [IdVar "a", IdVar "b", IdVar "c"])



parseTest :: Parsec String () a -> String -> Either String a
parseTest p s = either (Left . show) Right (parse p "" s)