module Tests where

import Test.Hspec
import VariableDeclarations ()

import qualified ArithmeticExpressionsSpec as ArithmeticExpressionsSpec
import qualified LexerSpec as LexerSpec
import qualified RelationalExpressionsSpec as RelationalExpressionsSpec
import qualified LogicalExpressionsSpec as LogicalExpressionsSpec
import qualified CommandsSpec as CommandsSpec
import qualified VariableDeclarationsSpec as VariableDeclarationsSpec
import qualified FunctionsAndParametersSpec as FunctionsAndParametersSpec

tests :: IO ()
tests = do
      hspec ArithmeticExpressionsSpec.spec
      hspec LexerSpec.spec
      hspec RelationalExpressionsSpec.spec
      hspec LogicalExpressionsSpec.spec
      hspec VariableDeclarationsSpec.spec
      hspec FunctionsAndParametersSpec.spec

      -- hspec CommandsSpec.spec