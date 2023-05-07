module RelationalExpressionsSpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Parsec (Parsec, parse)
import RelationalExpressions (relationalExpression)
import Types (Expr (..), ExprR (..), TCons (..))

-- Função auxiliar para executar testes de análise
parseTest :: Parsec String () a -> String -> Either String a
parseTest parser input = case parse parser "" input of
  Left err -> Left (show err)
  Right res -> Right res

spec :: Spec
spec = do
  describe "RelationalExpression" $ do
    it "parses less than" $
      parseTest relationalExpression "a < b" `shouldBe` Right (IdVar "a" :<: IdVar "b")

    it "parses greater than" $
      parseTest relationalExpression "a > b" `shouldBe` Right (IdVar "a" :>: IdVar "b")

    it "parses less than or equal" $
      parseTest relationalExpression "a <= b" `shouldBe` Right (IdVar "a" :<=: IdVar "b")

    it "parses greater than or equal" $
      parseTest relationalExpression "a >= b" `shouldBe` Right (IdVar "a" :>=: IdVar "b")

    it "parses equal" $
      parseTest relationalExpression "a == b" `shouldBe` Right (IdVar "a" :==: IdVar "b")

    it "parses not equal" $
      parseTest relationalExpression "a /= b" `shouldBe` Right (IdVar "a" :/=: IdVar "b")