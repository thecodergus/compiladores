module TestsAndDebugging where

import ProgramParser (programParser)
import Text.Parsec (parse)
import Types (Programa)

-- Função para testar seu analisador de programas
testParser :: String -> Either String Programa
testParser input = case parse programParser "" input of
  Left err -> Left $ "Error: " ++ show err
  Right result -> Right result

-- Função para exibir os resultados dos testes
printTestResult :: Either String Programa -> IO ()
printTestResult (Left errMsg) = putStrLn errMsg
printTestResult (Right program) = putStrLn $ "Parsed successfully: " ++ show program
