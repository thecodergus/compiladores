module Semantico.AnalisadorPrincipal where

import Sintatico.Types ( Funcao, FuncaoBloco, Programa(..), Var )
import Semantico.ErrosSemantico ( ErroSemantico (ChamadaFuncaoNaoDeclarada) )
import Semantico.AvisosSemantico ( AvisoSemantico )
import Semantico.AnalisadorVariaveis (analisarVariaveis)
import Semantico.AnalisadorFuncoes (analisarDeclaracoesFuncoes, encontrarFuncao, analisarChamadaFuncao)
import Semantico.AnalisadorComandos (analisarComandos)

-- | Função principal que analisa um programa
analisarPrograma :: Programa -> ([ErroSemantico], [AvisoSemantico], Programa)
analisarPrograma (Prog funcoes funcoesBlocos vars bloco) =
  let -- Analisar o bloco principal
      (errosBloco, avisosBloco, blocoModificado) = analisarComandos vars bloco

      -- Aqui você pode adicionar código para analisar as funções e seus blocos,
      -- por exemplo, percorrendo cada função e seu bloco correspondente.
      -- (errosFuncoes, avisosFuncoes, funcoesModificadas) = ...

      -- Combinar erros e avisos de blocos e funções
      erros = errosBloco -- ++ errosFuncoes (se você analisou funções)
      avisos = avisosBloco -- ++ avisosFuncoes (se você analisou funções)
   in (erros, avisos, Prog funcoes funcoesBlocos vars blocoModificado)
