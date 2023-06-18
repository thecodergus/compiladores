module Semantico.AnalisadorComandos where
import Sintatico.Types (Var ((:#:)), Bloco, Comando (Atrib, Leitura, Imp, Ret, Proc, If, While), Id, Expr (Chamada), Type (TDouble, TInt), Funcao ((:->:)), ExprL)
import Semantico.ErrosSemantico (ErroSemantico (IncompatibilidadeTipoAtribuicao, VariavelNaoDeclarada))
import Semantico.AvisosSemantico (AvisoSemantico (CoercaoTipo))
import Data.Foldable (find)
import Semantico.AnalisadorVariaveis (analisarAtribuicao, inferirTipo, analisarComandoAtribuicao)
import Data.Maybe (catMaybes, mapMaybe)


-- import 


-- Função que analisa um bloco de comandos
analisarComandos :: [Var] -> Bloco -> ([ErroSemantico], [AvisoSemantico], Bloco)
analisarComandos vars comandos = (erros, avisos, comandosModificados)
  where
    -- Aplica analisarComando a cada comando e extrai os erros, avisos e comandos modificados
    resultadosPorComando = map (analisarComando vars) comandos

    -- Extrai os erros e avisos de cada resultado
    erros = concatMap (\(errs, _, _) -> errs) resultadosPorComando
    avisos = concatMap (\(_, avs, _) -> avs) resultadosPorComando

    -- Extrai os comandos modificados
    comandosModificados = mapMaybe (\(_, _, maybeCmd) -> maybeCmd) resultadosPorComando

-- Nota: catMaybes é uma função de Data.Maybe que remove os Nothings e extrai os valores de Justs.


-- Função que analisa um comando individual
-- Ajustando a função analisarComando para chamar analisarComandoAtribuicao e retornar o comando modificado
analisarComando :: [Var] -> Comando -> ([ErroSemantico], [AvisoSemantico], Maybe Comando)
analisarComando vars comando =
  case comando of
    Atrib id expr -> analisarComandoAtribuicao vars [] [] comando
    Leitura id ->
      -- Aqui você pode chamar sua função analisarLeitura
      ([], [], Just comando)
    Imp expr ->
      -- Aqui você pode chamar sua função analisarImpressao
      ([], [], Just comando)
    Ret maybeExpr ->
      -- Aqui você pode chamar sua função analisarRetorno
      ([], [], Just comando)
    Proc id exprs ->
      -- Aqui você pode chamar sua função analisarChamadaProcedimento
      ([], [], Just comando)
    If exprL bloco1 bloco2 ->
      -- Aqui você pode chamar sua função analisarIf
      ([], [], Just comando)
    While exprL bloco ->
      -- Aqui você pode chamar sua função analisarWhile
      ([], [], Just comando)

-- Analisa a leitura de uma variável
analisarLeitura :: [Var] -> Id -> ([ErroSemantico], [AvisoSemantico])
analisarLeitura vars id = ([], [])

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
  ([], [])
-- Analisa um if
analisarIf :: [Var] -> ExprL -> Bloco -> Bloco -> ([ErroSemantico], [AvisoSemantico])
analisarIf vars exprL bloco1 bloco2 =
  ([], [])

-- Analisa um while
analisarWhile :: [Var] -> ExprL -> Bloco -> ([ErroSemantico], [AvisoSemantico])
analisarWhile vars exprL bloco =
  ([], [])