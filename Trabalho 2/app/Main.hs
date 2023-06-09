module Main where
import Text.Parsec ( parse )
import Sintatico.ProgramParser ( programParser )


main :: IO ()
main = do
    print "Digite o caminho para o arquivo de teste: "
    filePath <- getLine
    input <- readFile filePath
    case parse programParser "" input of
        Left err -> print err
        Right ast -> print ast
