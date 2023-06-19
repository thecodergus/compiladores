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
  let -- Função auxiliar para analisar um único bloco de função
      analisarFuncaoBloco (id, varsFuncao, blocoFuncao) =
        let (erros, avisos, blocoModificado) = analisarComandos funcoes (vars ++ varsFuncao) blocoFuncao
         in (erros, avisos, (id, varsFuncao, blocoModificado))

      -- Analisar o bloco principal
      (errosBloco, avisosBloco, blocoModificado) = analisarComandos funcoes vars bloco
      (errosVariaveis, avisosVariaveis, _) = analisarVariaveis vars bloco

      -- Analisar cada função bloco
      analiseFuncoesBlocos = map analisarFuncaoBloco funcoesBlocos
      (errosFuncoes, avisosFuncoes, funcoesBlocosModificados) = unzip3 analiseFuncoesBlocos

      -- Aanalisando as definicoes de funcoes
      errosDeclaracoesFuncoes = analisarDeclaracoesFuncoes funcoes

      -- Combinar erros e avisos de blocos e funções
      erros = errosBloco ++ concat errosFuncoes ++ errosVariaveis ++ errosDeclaracoesFuncoes
      avisos = avisosBloco ++ concat avisosFuncoes 
   in (erros, avisos, Prog funcoes funcoesBlocosModificados vars blocoModificado)
