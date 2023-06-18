module Semantico.AnalisadorPrincipal where

import Sintatico.Types ( Funcao, FuncaoBloco, Programa(..), Var )
import Semantico.ErrosSemantico ( ErroSemantico (ChamadaFuncaoNaoDeclarada) )
import Semantico.AvisosSemantico ( AvisoSemantico )
import Semantico.AnalisadorVariaveis (analisarVariaveis)
import Semantico.AnalisadorFuncoes (analisarDeclaracoesFuncoes, encontrarFuncao, analisarChamadaFuncao)
import Semantico.AnalisadorComandos (analisarComandos)

-- | Função principal que analisa um programa
analisarPrograma :: Programa -> ([ErroSemantico], [AvisoSemantico])
analisarPrograma (Prog funcoes funcoesBlocos vars bloco) = do
    analisarComandos vars bloco