module BlocksAndCommandListsSpec where

import BlocksAndCommandLists (block)
import Test.Hspec ( Spec, describe, it, shouldBe, Expectation )
import Types (Bloco, Comando (..), Expr (..), TCons (CInt))
import Text.Parsec (Parsec, parse)

spec :: Spec
spec = do
  describe "block" $ do
    it "parses an empty block" $ do
        parseTest block "{}" ([] :: Bloco)

    it "parses a block with a single command" $ do
      parseTest block "{ x = x + 1; }" [Atrib "x" ((:+:) (IdVar "x") (Const (CInt 1)))]

    it "parses a block with multiple commands" $ do
      parseTest block "{ x = x + 1; y = y - 1; }" [Atrib "x" ((:+:) (IdVar "x") (Const (CInt 1))), Atrib "y" ((:-:) (IdVar "y") (Const (CInt 1)))]



parseTest :: (Eq a, Show a) => Parsec String () a -> String -> a -> Expectation
parseTest parser input expected =
  case parse parser "" input of
    Left err -> fail $ "Parser failed with error: " ++ show err
    Right result -> result `shouldBe` expected