module Tests where

import qualified ArithmeticExpressionsSpec as ArithmeticExpressionsSpec
import qualified LexerSpec as LexerSpec
import Test.Hspec
import VariableDeclarations ()

tests :: IO ()
tests = do
  hspec ArithmeticExpressionsSpec.spec
  hspec LexerSpec.spec
