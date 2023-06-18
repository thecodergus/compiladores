module Main where
import Text.Parsec ( parse )
import Sintatico.ProgramParser ( programParser )
import Semantico.AnalisadorFuncoes
    ( analisarChamadaFuncao, analisarDeclaracoesFuncoes )

import Sintatico.Types
    ( Funcao((:->:)),
      Expr(Chamada, Const, IdVar),
      TCons(CDouble, CInt),
      Type(TInt, TDouble), Var ((:#:)) )
import Semantico.AnalisadorPrincipal (analisarPrograma)

main :: IO ()
main = do
    -- print "Digite o caminho para o arquivo de teste: "
    -- filePath <- getLine
    let filePath = "/home/gus/Documentos/compiladores/Trabalho 2/teste1.j";
    input <- readFile filePath
    case parse programParser "" input of
        Left err -> print err
        Right ast -> let (_, _, programa) = analisarPrograma ast
                            in print $ programa

-- main :: IO ()
-- main = do
--     let funcao1 = "soma" :->: (["a" :#: TInt, "b" :#: TInt], TInt)-- Definir algumas funções de amostra, variáveis e expressões
--     let funcao1 = "soma" :->: (["a" :#: TInt, "b" :#: TInt], TInt)
--     let funcao2 = "multiplica" :->: (["a" :#: TInt, "b" :#: TInt], TInt)
--     let funcao3 = "divisao" :->: (["a" :#: TDouble, "b" :#: TDouble], TDouble)
--     let funcao4 = "soma" :->: (["a" :#: TInt, "b" :#: TInt], TInt) -- Esta tem o mesmo nome que funcao1

--     let funcoes = [funcao1, funcao2, funcao3, funcao4]

--     let expr1 = Const (CInt 5)
--     let expr2 = Const (CDouble 6.0)
--     let expr3 = IdVar "a"
--     let expr4 = Chamada "soma" [expr1, expr2]
--     let expr5 = Chamada "soma" [expr1, expr1, expr1]
--     let expr6 = Chamada "multiplica" [expr1, expr1]
--     let expr7 = Chamada "naoExiste" [expr1]

--     let var1 = "a" :#: TInt
--     let var2 = "b" :#: TInt

--     let vars = [var1, var2]

--     -- Agora teste a função analisarChamadaFuncao

--     -- Esta chamada deve retornar erro de TiposParametrosIncompativel
--     print $ analisarChamadaFuncao funcoes vars expr4

--     -- Esta chamada deve retornar erro de NumeroParametrosIncorreto
--     print $ analisarChamadaFuncao funcoes vars expr5

--     -- Esta chamada deve passar, pois os tipos e número de argumentos estão corretos
--     print $ analisarChamadaFuncao funcoes vars expr6

--     -- Esta chamada deve retornar erro de ChamadaFuncaoNaoDeclarada
--     print $ analisarChamadaFuncao funcoes vars expr7

--     -- Teste a função analisarDeclaracoesFuncoes
--     -- Deve retornar um erro de FuncaoMultiplamenteDeclarada para "soma"
--     print $ analisarDeclaracoesFuncoes funcoes
