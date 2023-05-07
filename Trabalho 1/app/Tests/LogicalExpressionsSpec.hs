module LogicalExpressionsSpec where


import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Parsec (Parsec, parse)
import LogicalExpressions (logicalExpression)
import Types (Expr (..), ExprR (..), TCons (..), ExprL (..))

-- Função auxiliar para executar testes de análise
parseTest :: Parsec String () a -> String -> Either String a
parseTest parser input = case parse parser "" input of
  Left err -> Left (show err)
  Right res -> Right res

spec :: Spec
spec = do
  describe "LogicalExpression" $ do
    it "parses AND operator" $
      parseTest logicalExpression "(a < b) && b > c" `shouldBe` Right (Rel (IdVar "a" :<: IdVar "b") :&: Rel (IdVar "b" :>: IdVar "c"))

    it "parses OR operator" $
      parseTest logicalExpression "a < b || b > c" `shouldBe` Right (Rel (IdVar "a" :<: IdVar "b") :|: Rel (IdVar "b" :>: IdVar "c"))

    it "parses NOT operator" $
      parseTest logicalExpression "!(a < b)" `shouldBe` Right (Not (Rel (IdVar "a" :<: IdVar "b")))