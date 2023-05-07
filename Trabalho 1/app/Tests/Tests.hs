module Tests where

import Test.Hspec
import VariableDeclarations ()

import qualified ArithmeticExpressionsSpec as ArithmeticExpressionsSpec
import qualified LexerSpec as LexerSpec
import qualified RelationalExpressionsSpec as RelationalExpressionsSpec
import qualified LogicalExpressionsSpec as LogicalExpressionsSpec
import qualified CommandsSpec as CommandsSpec


tests :: IO ()
tests = do
      hspec ArithmeticExpressionsSpec.spec
      hspec LexerSpec.spec
      hspec RelationalExpressionsSpec.spec
      hspec LogicalExpressionsSpec.spec
      -- hspec CommandsSpec.spec