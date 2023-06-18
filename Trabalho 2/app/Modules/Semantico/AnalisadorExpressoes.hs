module Semantico.AnalisadorExpressoes where

import Sintatico.Types (Var ((:#:)), Funcao, Expr (..), TCons(..), Type(..), Comando (Leitura), Id, )
import Semantico.ErrosSemantico (ErroSemantico (TiposIncompativeis, VariavelNaoDeclarada))
import Semantico.AnalisadorVariaveis (inferirTipo, analisarVariaveis)
import Semantico.AnalisadorFuncoes (analisarChamadaFuncao)
import Semantico.AvisosSemantico (AvisoSemantico(..))


-- Função principal para analisar uma expressão
analisarExpressao :: [Var] -> [Funcao] -> Expr -> ([ErroSemantico], [AvisoSemantico])
analisarExpressao vars funcoes expr = case expr of
  Const c -> ([], [])
  IdVar id -> analisarUsoVariavel vars id
  Chamada id exprs -> analisarChamadaFuncao funcoes vars (Chamada id exprs)
  e1 :+: e2 -> analisarOperacaoBinaria vars funcoes (:+:) e1 e2
  e1 :-: e2 -> analisarOperacaoBinaria vars funcoes (:-:) e1 e2
  e1 :*: e2 -> analisarOperacaoBinaria vars funcoes (:*:) e1 e2
  e1 :/: e2 -> analisarOperacaoBinaria vars funcoes (:/:) e1 e2
  Neg e -> analisarExpressao vars funcoes e
  Lit _ -> ([], [])


-- | Função para analisar o uso de uma variável.
-- Esta função recebe uma lista de variáveis 'vars' que estão no escopo atual,
-- e um identificador 'id' que representa a variável cujo uso queremos analisar.
-- A função retorna uma lista de erros semânticos e uma lista de avisos semânticos.
-- Se a variável com o identificador 'id' não estiver na lista 'vars',
-- um erro será retornado indicando que a variável não foi declarada.
analisarUsoVariavel :: [Var] -> Id -> ([ErroSemantico], [AvisoSemantico])
analisarUsoVariavel vars id =
  let varsWithId = filter (\(varId :#: _) -> varId == id) vars
   in if null varsWithId -- Verifica se a lista filtrada está vazia (ou seja, variável não encontrada)
        then ([VariavelNaoDeclarada id], []) -- Erro: variável não declarada
        else ([], []) -- Nenhum erro ou aviso



  

-- Função auxiliar para analisar operações binárias
analisarOperacaoBinaria :: [Var] -> [Funcao] -> (Expr -> Expr -> Expr) -> Expr -> Expr -> ([ErroSemantico], [AvisoSemantico])
analisarOperacaoBinaria vars funcoes op e1 e2 =
  let t1 = inferirTipo vars e1
      t2 = inferirTipo vars e2
      (erros1, avisos1) = analisarExpressao vars funcoes e1
      (erros2, avisos2) = analisarExpressao vars funcoes e2
   in case (t1, t2) of
        (TString, TString) -> (erros1 ++ erros2, avisos1 ++ avisos2)
        (TInt, TDouble) -> (erros1 ++ erros2, ConversaoAutomaticaExpr e1 TInt TDouble : avisos1 ++ avisos2)
        (TDouble, TInt) -> (erros1 ++ erros2, ConversaoAutomaticaExpr e2 TInt TDouble : avisos1 ++ avisos2)
        (TString, _) -> (TiposIncompativeis t1 t2 : erros1 ++ erros2, avisos1 ++ avisos2)
        (_, TString) -> (TiposIncompativeis t1 t2 : erros1 ++ erros2, avisos1 ++ avisos2)
        (TVoid, _) -> (TiposIncompativeis t1 t2 : erros1 ++ erros2, avisos1 ++ avisos2)
        (_, TVoid) -> (TiposIncompativeis t1 t2 : erros1 ++ erros2, avisos1 ++ avisos2)
        _ ->
          if t1 == t2
            then (erros1 ++ erros2, avisos1 ++ avisos2)
            else (TiposIncompativeis t1 t2 : erros1 ++ erros2, avisos1 ++ avisos2)
