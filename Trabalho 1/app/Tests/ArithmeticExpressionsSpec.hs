module ArithmeticExpressionsSpec where

import ArithmeticExpressions (arithmeticExpression)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Text.Parsec (parse)
import Types (Expr (..), TCons (..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "arithmeticExpression" $ do
    it "parses integer constants" $
      parse arithmeticExpression "" "42" `shouldBe` Right (Const (CInt 42))

    -- it "parses double constants" $
    --   parse arithmeticExpression "" "3.14" `shouldBe` Right (Const (CDouble 3.14))

    it "parses identifiers" $
      parse arithmeticExpression "" "x" `shouldBe` Right (IdVar "x")

    it "parses addition" $
      parse arithmeticExpression "" "1 + 2" `shouldBe` Right ((Const (CInt 1)) :+: (Const (CInt 2)))

    it "parses subtraction" $
      parse arithmeticExpression "" "3 - 4" `shouldBe` Right ((Const (CInt 3)) :-: (Const (CInt 4)))

    it "parses multiplication" $
      parse arithmeticExpression "" "5 * 6" `shouldBe` Right ((Const (CInt 5)) :*: (Const (CInt 6)))

    it "parses division" $
      parse arithmeticExpression "" "7 / 8" `shouldBe` Right ((Const (CInt 7)) :/: (Const (CInt 8)))

    it "parses complex expressions" $
      parse arithmeticExpression "" "(3 + 5) * 2 - 1" `shouldBe` Right ((((Const (CInt 3)) :+: (Const (CInt 5))) :*: (Const (CInt 2))) :-: (Const (CInt 1)))
