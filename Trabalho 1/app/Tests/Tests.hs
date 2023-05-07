module Tests where

import qualified ArithmeticExpressionsSpec as ArithmeticExpressionsSpec
import Test.Hspec
import VariableDeclarations ()

tests :: IO ()
tests = do
  hspec ArithmeticExpressionsSpec.spec
