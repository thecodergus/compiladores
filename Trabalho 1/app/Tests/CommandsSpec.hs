module CommandsSpec where

import Test.Hspec
import Text.Parsec (Parsec, parse)
import Commands (command)
import Types (Expr(..), TCons(..), ExprL(..), ExprR(..), Comando(..), Bloco(..))

spec :: Spec
spec = do
  describe "Commands" $ do
    it "parses if command" $ do
      parseTest command "if (x < y) { x = x + 1; } else { y = y + 1; }" `shouldBe` Right (If (Rel (IdVar "a" :<: IdVar "b")) [Atrib "x" ((:+:) (IdVar "x") (Const (CInt 1)))] [Atrib "y" ((:+:) (IdVar "y") (Const (CInt 1)))])
    
    it "parses if command" $ do
      parseTest command "if (x < y) {} else {}" `shouldBe` Right (If (Rel (IdVar "a" :<: IdVar "b")) [] [])

    it "parses while command" $ do
      parseTest command "while (x < 10) { x = x + 1; }" `shouldBe` Right (While (Rel (IdVar "x" :<: Const (CInt 10))) [Atrib "x" ((:+:) (IdVar "x") (Const (CInt 1)))])
    
    it "parses while command" $ do
      parseTest command "while (x < 10) { x = x + 1; }" `shouldBe` Right (While (Rel (IdVar "x" :<: Const (CInt 10))) [])

    -- it "parses atrib command" $ do
    --   parseTest command "x = x + 1;" `shouldBe` Right (Atrib "x" (Add (Var "x") (Const 1)))

    -- it "parses read command" $ do
    --   parseTest command "read x;" `shouldBe` Right (Leitura "x")

    -- it "parses print command" $ do
    --   parseTest command "print x + 1;" `shouldBe` Right (Imp (Add (Var "x") (Const 1)))

    -- it "parses return command with expression" $ do
    --   parseTest command "return x + 1;" `shouldBe` Right (Ret (Just (Add (Var "x") (Const 1))))

    -- it "parses return command without expression" $ do
    --   parseTest command "return;" `shouldBe` Right (Ret Nothing)

    -- it "parses proc command" $ do
    --   parseTest command "myFunc(x, y, z);" `shouldBe` Right (Proc "myFunc" [Var "x", Var "y", Var "z"])



parseTest :: Parsec String () a -> String -> Either String a
parseTest p s = either (Left . show) Right (parse p "" s)