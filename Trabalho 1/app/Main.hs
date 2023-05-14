import ProgramParser (programParser)
import Text.Parsec (parse)


main :: IO ()
main = do
  print $ parse programParser "" "void main(int a, int b); void main(int a, int b){a= 2;}"