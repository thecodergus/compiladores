module Semantico.AnalisadorComandos where
import Sintatico.Types (Var ((:#:)), Bloco, Comando (Atrib, Leitura, Imp, Ret, Proc, If, While), Id, Expr (Chamada), Type (TDouble, TInt), Funcao ((:->:)), ExprL)
import Semantico.ErrosSemantico (ErroSemantico (IncompatibilidadeTipoAtribuicao, VariavelNaoDeclarada))
import Semantico.AvisosSemantico (AvisoSemantico (CoercaoTipo))
import Data.Foldable (find)
import Semantico.AnalisadorVariaveis (analisarAtribuicao, inferirTipo, analisarComandoAtribuicao)

-- import 


-- Função que analisa um bloco de comandos
analisarComandos :: [Var] -> Bloco -> ([ErroSemantico], [AvisoSemantico])
analisarComandos vars comandos = foldl combinar ([], []) resultadosRelevantes
  where
    -- Aplica analisarComando a cada comando, mas mantém apenas os erros e avisos (ignora o Maybe Comando)
    resultadosRelevantes = map ((\(erros, avisos, _) -> (erros, avisos)) . analisarComando vars) comandos

    -- Função auxiliar para combinar os resultados
    combinar :: ([ErroSemantico], [AvisoSemantico]) -> ([ErroSemantico], [AvisoSemantico]) -> ([ErroSemantico], [AvisoSemantico])
    combinar (errosAcc, avisosAcc) (erros, avisos) = (errosAcc ++ erros, avisosAcc ++ avisos)


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