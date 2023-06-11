module Semantico.AnalisadorVariaveis where


import Semantico.AvisosSemantico (AvisoSemantico (..))
import Semantico.ErrosSemantico (ErroSemantico (..))
import Sintatico.Types (Bloco, Comando (..), Id, Type (..), Var (..), Expr (..), TCons (CDouble, CInt))
import Data.Foldable (find)

-- | Função que analisa o bloco de comandos em busca de erros relacionados às variáveis.
-- Recebe uma lista de variáveis e um bloco de comandos, e retorna uma lista de erros semânticos e avisos.
analisarVariaveis :: [Var] -> Bloco -> ([ErroSemantico], [AvisoSemantico])
analisarVariaveis vars = foldl (analisarComando vars) ([], [])
  where
    analisarComando vars (erros, avisos) = analisarComandoAtribuicao vars erros avisos

-- | Função que analisa um comando de atribuição dentro de um bloco e acumula erros e avisos.
-- Recebe uma lista de variáveis, uma lista de erros acumulados, uma lista de avisos acumulados, e um comando,
-- e retorna uma lista de erros e avisos acumulados.
analisarComandoAtribuicao :: [Var] -> [ErroSemantico] -> [AvisoSemantico] -> Comando -> ([ErroSemantico], [AvisoSemantico])
analisarComandoAtribuicao vars erros avisos (Atrib id expr) =
  let (errs, avs) = analisarAtribuicao vars id expr
   in (erros ++ errs, avisos ++ avs)
analisarComandoAtribuicao _ erros avisos _ = (erros, avisos)


-- | Função que analisa um comando de atribuição.
-- Recebe uma lista de variáveis, um identificador e uma expressão, e retorna uma lista de erros semânticos e avisos.
analisarAtribuicao :: [Var] -> Id -> Expr -> ([ErroSemantico], [AvisoSemantico])
analisarAtribuicao vars id expr =
  case encontrarVariavel vars id of
    Just (varId :#: varType) ->
      let exprType = inferirTipo vars expr
       in gerarErrosAvisosAtribuicao varId varType exprType
    Nothing -> ([VariavelNaoDeclarada id], [])

-- | Função para analisar se uma variável foi declarada.
-- Recebe uma lista de variáveis e um identificador, retorna a variável se encontrada ou Nothing caso contrário.
encontrarVariavel :: [Var] -> Id -> Maybe Var
encontrarVariavel vars id = find (\(vid :#: _) -> vid == id) vars

-- | Função para gerar erros ou avisos com base nos tipos envolvidos na atribuição.
-- Recebe o identificador da variável, o tipo da variável, e o tipo da expressão a ser atribuída.
gerarErrosAvisosAtribuicao :: Id -> Type -> Type -> ([ErroSemantico], [AvisoSemantico])
gerarErrosAvisosAtribuicao varId varType exprType =
  case (varType, exprType) of
    (TDouble, TInt) -> ([IncompatibilidadeTipoAtribuicao varId TDouble TInt], [CoercaoTipo TInt TDouble, ConversaoAutomatica TInt TDouble])
    (TInt, TDouble) -> ([IncompatibilidadeTipoAtribuicao varId TInt TDouble], [CoercaoTipo TDouble TInt, ConversaoAutomatica TDouble TInt])
    _ -> if varType == exprType then ([], []) else ([IncompatibilidadeTipoAtribuicao varId varType exprType], [])


-- Função para inferir o tipo de uma expressão
inferirTipo :: [Var] -> Expr -> Type
inferirTipo vars expr =
  case expr of
    Const constante -> inferirTipoConstante constante
    IdVar id -> inferirTipoVariavel vars id
    expr1 :+: expr2 -> inferirTipoBinOp vars expr1 expr2
    expr1 :-: expr2 -> inferirTipoBinOp vars expr1 expr2
    expr1 :*: expr2 -> inferirTipoBinOp vars expr1 expr2
    expr1 :/: expr2 -> inferirTipoBinOp vars expr1 expr2
    _ -> TVoid -- Casos adicionais devem ser tratados aqui

-- | Função para inferir o tipo de uma constante.
-- Recebe uma constante (TCons) e retorna o tipo correspondente.
inferirTipoConstante :: TCons -> Type
inferirTipoConstante constante = case constante of
  CDouble _ -> TDouble -- Se for uma constante do tipo Double, retorna TDouble
  CInt _ -> TInt -- Se for uma constante do tipo Int, retorna TInt

-- | Função para inferir o tipo de uma variável baseado em seu identificador.
-- Recebe uma lista de variáveis e um identificador, e retorna o tipo da variável correspondente.
-- Se a variável não estiver na lista, retorna TVoid como um placeholder para representar um erro.
inferirTipoVariavel :: [Var] -> Id -> Type
inferirTipoVariavel vars id =
  case find (\(vid :#: _) -> vid == id) vars of
    Just (_ :#: tipo) -> tipo -- Se a variável foi encontrada, retorna o seu tipo
    Nothing -> TVoid -- Caso contrário, retorna TVoid como um placeholder para erro

-- | Função para inferir o tipo resultante de uma operação binária.
-- Recebe uma lista de variáveis e duas expressões (operandos da operação binária),
-- e retorna o tipo resultante da operação. Se os tipos dos operandos forem diferentes,
-- retorna TVoid como um placeholder para representar um erro.
inferirTipoBinOp :: [Var] -> Expr -> Expr -> Type
inferirTipoBinOp vars expr1 expr2 =
  let t1 = inferirTipo vars expr1 -- Inferir o tipo do primeiro operando
      t2 = inferirTipo vars expr2 -- Inferir o tipo do segundo operando
   in if t1 == t2
        then t1 -- Se os tipos forem iguais, retorna o tipo comum
        else TVoid -- Caso contrário, retorna TVoid como um placeholder para erro



-- Função auxiliar para obter o nome de uma variável
nomeVariavel :: Var -> String
nomeVariavel (id :#: _) =  id