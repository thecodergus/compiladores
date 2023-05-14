import ProgramParser (programParser)
import Text.Parsec (parse)


main :: IO ()
main = do
  print $ parse programParser "" "void main(int a, int b); void main(int a, int b){return 0;}"
  print $ parse programParser "" "void main(int a, int b); void main(int a, int b){int a; a = 2;}"
  print $ parse programParser "" "void main(int a, int b); void main(int a, int b){int a; a = 2;} int b(){return 2;} int c(){}"
  print $ parse programParser "" "int A, B; void main(int a, int b); void main(int a, int b){int a; a = 2;} int b(){return 2;} int c(){}"
  print $ parse programParser "" "int A, B; void main(){A = 2;}"
  print $ parse programParser "" "int A, B; void main(){if(A == 2){A = 3;}}"
  print $ parse programParser "" "void main(int a, int b); void main(int a, int b){int a; a = 2;} int b(){return 2;} int c(){b(); return 2;}"
  print $ parse programParser "" "void main(int a, int b); void main(int a, int b){} int b(){return 2;} int c(){b(); return 2;}"