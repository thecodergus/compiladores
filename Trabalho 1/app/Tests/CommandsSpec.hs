module CommandsSpec where

import Test.Hspec
import Text.Parsec (Parsec, parse)
import Commands (command)
import Types (Expr(..), TCons(..), ExprL(..), ExprR(..), Comando(..), Bloco(..))

-- main :: IO ()
-- main = do 

spec :: Spec
spec = do
  describe "Commands" $ do
    it "parses assignment command" $ do
      parseTest command "a = 10;" `shouldBe` Right (Atrib "a" (Const (CInt 10)))

    it "parses print command" $ do
      parseTest command "print(a);" `shouldBe` Right (Imp (IdVar "a"))
    
    it "parses atribCommand" $ do
      parseTest command "a = b + 1;" `shouldBe`
        Right (Atrib "a" (IdVar "b" :+: Const (CInt 1)))

    -- it "parses if command" $ do
    --   parseTest command "if (a > b){ a = 20; } else{ a = 10; }" `shouldBe`
    --     Right (If (Rel (IdVar "a" :>: IdVar "b")) (Atrib "a" (Const (CInt 20))) (Atrib "a" (Const (CInt 10))))

    -- it "parses while command" $ do
    --   parseTest command "while (a < 10) a = a + 1;" `shouldBe`
    --     Right (While (Rel (IdVar "a" :<: Const (CInt 10))) (Atrib "a" (IdVar "a" :+: Const (CInt 1))))


    -- it "parses read command" $ do
    --   parseTest command "read(a);" `shouldBe` Right (Leitura (IdVar "a"))


    -- it "parses returnCommand" $ do
    --   parseTest command "return a + 1;" `shouldBe`
    --     Right (Ret (IdVar "a" :+: Const (CInt 1)))

    -- it "parses procCommand" $ do
    --   parseTest command "myFunction(a, b, c);" `shouldBe`
    --     Right (Proc "myFunction" [IdVar "a", IdVar "b", IdVar "c"])



parseTest :: Parsec String () a -> String -> Either String a
parseTest p s = either (Left . show) Right (parse p "" s)