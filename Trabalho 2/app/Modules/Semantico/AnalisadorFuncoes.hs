module Semantico.AnalisadorFuncoes where
import Sintatico.Types (Id, Type, Funcao ((:->:)), Expr (Chamada), Var ((:#:)))
import Semantico.ErrosSemantico (ErroSemantico (..))
import Semantico.AvisosSemantico (AvisoSemantico (..))
import Data.Foldable (find)
import Semantico.AnalisadorVariaveis (inferirTipo)
import Data.List (sortOn, groupBy)
import Data.Maybe (catMaybes)

-- | Analisa uma chamada de função para verificar se ela foi declarada, 
-- | se a quantidade de parâmetros é correta e se os tipos de parâmetros são compatíveis.
analisarChamadaFuncao :: [Funcao] -> [Var] -> Expr -> ([ErroSemantico], [AvisoSemantico])
analisarChamadaFuncao funcoes vars (Chamada id exprs) =
  case encontrarFuncao funcoes id of
    Just (_ :->: (params, _)) ->
      let paramTipos = tiposParametros params
          argTipos = map (inferirTipo vars) exprs
      in analisarParametros id paramTipos argTipos
    Nothing -> ([ChamadaFuncaoNaoDeclarada id], [])
  where
    -- | Obtém os tipos dos parâmetros de uma função
    tiposParametros = map (\(_ :#: tipo) -> tipo)
    -- | Procura uma função pelo identificador em uma lista de funções
    encontrarFuncao funcoes id = find (\(fid :->: _) -> fid == id) funcoes

-- | Compara o número de parâmetros e os tipos de uma chamada de função com a declaração
analisarParametros :: Id -> [Type] -> [Type] -> ([ErroSemantico], [AvisoSemantico])
analisarParametros id paramTipos argTipos
  | length paramTipos /= length argTipos = ([NumeroParametrosIncorreto id], [])
  | paramTipos == argTipos = ([], [])
  | otherwise = (encontrarIncompatibilidades id paramTipos argTipos, [])

-- Função auxiliar que encontra incompatibilidades entre tipos de parâmetros
encontrarIncompatibilidades :: Id -> [Type] -> [Type] -> [ErroSemantico]
encontrarIncompatibilidades id paramTipos argTipos =
  [TiposParametrosIncompativel id esperado encontrado | (esperado, encontrado) <- zip paramTipos argTipos, esperado /= encontrado]

-- | Analisa uma lista de declarações de funções para identificar funções declaradas várias vezes
analisarDeclaracoesFuncoes :: [Funcao] -> [ErroSemantico]
analisarDeclaracoesFuncoes funcoes =
  let duplicatas = encontrarDuplicatas funcoes
  in map funcaoMultiplamenteDeclarada duplicatas
  where
    -- | Cria um erro de função declarada várias vezes para um grupo de funções com o mesmo nome
    funcaoMultiplamenteDeclarada grupo = FuncaoMultiplamenteDeclarada (getId $ head grupo)
    -- | Obtém o identificador de uma função
    getId (fid :->: _) = fid
    -- | Encontra grupos de funções com o mesmo nome
    encontrarDuplicatas funcoes =
      let sortedFuncoes = sortOn (\(fid :->: _) -> fid) funcoes
          grupos = groupBy (\(fid1 :->: _) (fid2 :->: _) -> fid1 == fid2) sortedFuncoes
      in filter ((> 1) . length) grupos

-- | Analisa uma lista de declarações de funções para identificar funções que não possuem retorno
analisarRetornoFuncao :: [Var] -> Funcao -> Expr -> ([ErroSemantico], [AvisoSemantico])
analisarRetornoFuncao vars (id :->: (_, tipoRetornoEsperado)) exprRetorno =
  let tipoRetornoEncontrado = inferirTipo vars exprRetorno
   in if tipoRetornoEsperado == tipoRetornoEncontrado
        then ([], [])
        else ([IncompatibilidadeTipoRetorno id tipoRetornoEsperado tipoRetornoEncontrado], [])


-- Função para encontrar uma função pelo nome
encontrarFuncao :: Id -> [Funcao] -> Maybe Funcao
encontrarFuncao nome = find (\(id :->: _) -> id == nome)