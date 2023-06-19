module Semantico.AnalisadorComandos where
import Sintatico.Types (Var ((:#:)), Bloco, Comando (Atrib, Leitura, Imp, Ret, Proc, If, While), Id, Expr (Chamada), Type (TDouble, TInt), Funcao ((:->:)), ExprL)
import Semantico.ErrosSemantico (ErroSemantico (IncompatibilidadeTipoAtribuicao, VariavelNaoDeclarada, ChamadaFuncaoNaoDeclarada, NumeroParametrosIncorreto))
import Semantico.AvisosSemantico (AvisoSemantico (CoercaoTipo))
import Data.Foldable (find)
import Semantico.AnalisadorVariaveis (analisarAtribuicao, inferirTipo, analisarComandoAtribuicao)
import Data.Maybe (catMaybes, mapMaybe)


-- import 


-- Função que analisa um bloco de comandos
analisarComandos :: [Funcao] -> [Var] -> Bloco -> ([ErroSemantico], [AvisoSemantico], Bloco)
analisarComandos funcoes vars comandos = (erros, avisos, comandosModificados)
  where
    -- Aplica analisarComando a cada comando e extrai os erros, avisos e comandos modificados
    resultadosPorComando = map (analisarComando vars funcoes) comandos

    -- Extrai os erros e avisos de cada resultado
    erros = concatMap (\(errs, _, _) -> errs) resultadosPorComando
    avisos = concatMap (\(_, avs, _) -> avs) resultadosPorComando

    -- Extrai os comandos modificados
    comandosModificados = mapMaybe (\(_, _, maybeCmd) -> maybeCmd) resultadosPorComando



-- Função que analisa um comando individual
-- Ajustando a função analisarComando para chamar analisarComandoAtribuicao e retornar o comando modificado
-- Função que analisa um comando individual
analisarComando :: [Var] -> [Funcao] -> Comando -> ([ErroSemantico], [AvisoSemantico], Maybe Comando)
analisarComando vars funcoes comando =
  case comando of
    Atrib id expr -> analisarComandoAtribuicao vars [] [] comando
    Leitura id ->
      let (erros, avisos) = analisarLeitura vars id
       in (erros, avisos, Just comando)
    Imp expr ->
      let (erros, avisos) = analisarImpressao vars expr
       in (erros, avisos, Just comando)
    Ret maybeExpr ->
      let (erros, avisos) = analisarRetorno vars maybeExpr
       in (erros, avisos, Just comando)
    Proc id exprs ->
      let (erros, avisos) = analisarChamadaProcedimento vars funcoes id exprs
       in (erros, avisos, Just comando)
    If exprL bloco1 bloco2 ->
      let (erros, avisos) = analisarIf vars exprL bloco1 bloco2
       in (erros, avisos, Just comando)
    While exprL bloco ->
      let (erros, avisos) = analisarWhile vars exprL bloco
       in (erros, avisos, Just comando)

-- Analisa a leitura de uma variável
analisarLeitura :: [Var] -> Id -> ([ErroSemantico], [AvisoSemantico])
analisarLeitura vars id =
  if variavelDeclarada id vars
    then ([], [])
    else ([VariavelNaoDeclarada id], [])


-- Analisa uma impressão
analisarImpressao :: [Var] -> Expr -> ([ErroSemantico], [AvisoSemantico])
analisarImpressao vars expr =
  -- Este exemplo apenas retorna uma estrutura vazia, adicione sua lógica de análise aqui
  ([], [])

-- Analisa um retorno
analisarRetorno :: [Var] -> Maybe Expr -> ([ErroSemantico], [AvisoSemantico])
analisarRetorno vars maybeExpr =
  -- Este exemplo apenas retorna uma estrutura vazia, adicione sua lógica de análise aqui
  ([], [])

-- Analisa uma chamada de procedimento
analisarChamadaProcedimento :: [Var] -> [Funcao] -> Id -> [Expr] -> ([ErroSemantico], [AvisoSemantico])
analisarChamadaProcedimento vars funcoes id exprs =
  case funcaoDeclarada id funcoes of
    Just (params, returnType) ->
      if length params == length exprs
        then ([], []) -- Aqui você deve adicionar mais análises para os tipos dos parâmetros
        else ([NumeroParametrosIncorreto id], [])
    Nothing -> ([ChamadaFuncaoNaoDeclarada id], [])

-- Analisa um if
analisarIf :: [Var] -> ExprL -> Bloco -> Bloco -> ([ErroSemantico], [AvisoSemantico])
analisarIf vars exprL bloco1 bloco2 =
  ([], [])

-- Analisa um while
analisarWhile :: [Var] -> ExprL -> Bloco -> ([ErroSemantico], [AvisoSemantico])
analisarWhile vars exprL bloco =
  ([], [])


variavelDeclarada :: Id -> [Var] -> Bool
variavelDeclarada id = any (\(varId :#: _) -> varId == id)

funcaoDeclarada :: Id -> [Funcao] -> Maybe ([Var], Type)
funcaoDeclarada id funcoes =
  case find (\(funId :->: _) -> funId == id) funcoes of
    Just (_ :->: (params, returnType)) -> Just (params, returnType)
    Nothing -> Nothing


