import TestsAndDebugging

main :: IO ()
main = do
  let exampleProgram = "int a = 0; double c = 2;"
  let result = testParser exampleProgram
  printTestResult result
