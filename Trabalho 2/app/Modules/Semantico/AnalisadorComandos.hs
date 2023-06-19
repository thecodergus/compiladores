module Semantico.AnalisadorComandos where
import Sintatico.Types (Var ((:#:)), Bloco, Comando (Atrib, Leitura, Imp, Ret, Proc, If, While), Id, Expr (Chamada, ConverterPara), Type (TDouble, TInt, TVoid, TString), Funcao ((:->:)), ExprL (Not, Rel, (:&:), (:|:)), ExprR ((:==:), (:/=:), (:>:), (:<:), (:<=:), (:>=:)))
import Semantico.ErrosSemantico (ErroSemantico (IncompatibilidadeTipoAtribuicao, VariavelNaoDeclarada, ChamadaFuncaoNaoDeclarada, NumeroParametrosIncorreto, TipoInvalidoImpressao, TiposIncompativeis))
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
    -- Ret maybeExpr ->
    --   let (erros, avisos) = analisarRetorno vars maybeExpr -- Verifique a assinatura de analisarRetorno e ajuste se necessário
    --    in (erros, avisos, Just comando)
    -- Ret maybeExpr ->
      -- let (erros, avisos) = analisarRetorno vars maybeExpr TVoid -- Verifique a assinatura de analisarRetorno e ajuste se necessário
      --  in (erros, avisos, Just comando)
    Ret maybeExpr -> ([], [], Nothing)
    Proc id exprs ->
      let (erros, avisos) = analisarChamadaProcedimento vars funcoes id exprs
       in (erros, avisos, Just comando)
    If exprL bloco1 bloco2 ->
      let (erros, avisos) = analisarIf funcoes vars exprL bloco1 bloco2
       in (erros, avisos, Just comando)
    While exprL bloco ->
      let (erros, avisos) = analisarWhile funcoes vars exprL bloco
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
  let exprType = inferirTipo vars expr
   in if isPrintableType exprType -- isPrintableType é uma função que você deve implementar
        then ([], [])
        else ([TipoInvalidoImpressao exprType], [])

-- Analisa um retorno
analisarRetorno :: [Var] -> Maybe Expr -> Type -> ([ErroSemantico], [AvisoSemantico])
analisarRetorno vars maybeExpr expectedReturnType =
  case maybeExpr of
    Just expr ->
      let exprType = inferirTipo vars expr
       in if exprType == expectedReturnType
            then ([], [])
            else ([IncompatibilidadeTipoAtribuicao "função" expectedReturnType exprType], [])
    Nothing -> ([], [])


-- Analisa uma chamada de procedimento
analisarChamadaProcedimento :: [Var] -> [Funcao] -> Id -> [Expr] -> ([ErroSemantico], [AvisoSemantico])
analisarChamadaProcedimento vars funcoes id exprs =
  case funcaoDeclarada id funcoes of
    Just (params, returnType) ->
      if length params == length exprs
        then ([], []) -- Aqui você deve adicionar mais análises para os tipos dos parâmetros
        else ([NumeroParametrosIncorreto id], [])
    Nothing -> ([ChamadaFuncaoNaoDeclarada id], [])

-- Atualização da função analisarIf
analisarIf :: [Funcao] -> [Var] -> ExprL -> Bloco -> Bloco -> ([ErroSemantico], [AvisoSemantico])
analisarIf funcoes vars exprL bloco1 bloco2 =
  let (errosExpr, avisosExpr, _) = analisarExpressaoBooleana vars exprL
      (errosBloco1, avisosBloco1, _) = analisarComandos funcoes vars bloco1
      (errosBloco2, avisosBloco2, _) = analisarComandos funcoes vars bloco2
   in (errosExpr ++ errosBloco1 ++ errosBloco2, avisosExpr ++ avisosBloco1 ++ avisosBloco2)

-- Atualização da função analisarWhile
analisarWhile :: [Funcao] -> [Var] -> ExprL -> Bloco -> ([ErroSemantico], [AvisoSemantico])
analisarWhile funcoes vars exprL bloco =
  let (errosExpr, avisosExpr, _) = analisarExpressaoBooleana vars exprL
      (errosBloco, avisosBloco, _) = analisarComandos funcoes vars bloco
   in (errosExpr ++ errosBloco, avisosExpr ++ avisosBloco)


variavelDeclarada :: Id -> [Var] -> Bool
variavelDeclarada id = any (\(varId :#: _) -> varId == id)

funcaoDeclarada :: Id -> [Funcao] -> Maybe ([Var], Type)
funcaoDeclarada id funcoes =
  case find (\(funId :->: _) -> funId == id) funcoes of
    Just (_ :->: (params, returnType)) -> Just (params, returnType)
    Nothing -> Nothing



-- Função fictícia para analisar se uma expressão é booleana, você precisa implementá-la corretamente.
analisarExpressaoBooleana :: [Var] -> ExprL -> ([ErroSemantico], [AvisoSemantico], Maybe ExprL)
analisarExpressaoBooleana vars exprL =
  case exprL of
    Not expr -> analisarExpressaoBooleana vars expr
    Rel exprR -> analisarExpressaoRelacional vars exprR
    leftExprL :&: rightExprL -> mergeAnalises (analisarExpressaoBooleana vars leftExprL) (analisarExpressaoBooleana vars rightExprL)
    leftExprL :|: rightExprL -> mergeAnalises (analisarExpressaoBooleana vars leftExprL) (analisarExpressaoBooleana vars rightExprL)

analisarExpressaoRelacional :: [Var] -> ExprR -> ([ErroSemantico], [AvisoSemantico], Maybe ExprL)
analisarExpressaoRelacional vars exprR =
  case exprR of
    leftExpr :==: rightExpr -> analisarBinOp vars leftExpr rightExpr
    leftExpr :/=: rightExpr -> analisarBinOp vars leftExpr rightExpr
    leftExpr :<: rightExpr -> analisarBinOp vars leftExpr rightExpr
    leftExpr :>: rightExpr -> analisarBinOp vars leftExpr rightExpr
    leftExpr :<=: rightExpr -> analisarBinOp vars leftExpr rightExpr
    leftExpr :>=: rightExpr -> analisarBinOp vars leftExpr rightExpr

analisarBinOp :: [Var] -> Expr -> Expr -> ([ErroSemantico], [AvisoSemantico], Maybe ExprL)
analisarBinOp vars leftExpr rightExpr =
  let leftType = tipoDeExpressao vars leftExpr
      rightType = tipoDeExpressao vars rightExpr
      isCompatible =
        (leftType == TString && rightType == TString)
          || ((leftType == TInt || leftType == TDouble) && (rightType == TInt || rightType == TDouble))
      errosOp = ([TiposIncompativeis leftType rightType | not isCompatible])
      convertedExpr
        | leftType == TInt && rightType == TDouble = Just (Rel (ConverterPara TDouble leftExpr :==: rightExpr))
        | leftType == TDouble && rightType == TInt = Just (Rel (leftExpr :==: ConverterPara TDouble rightExpr))
        | otherwise = Nothing
   in (errosOp, [], convertedExpr)

mergeAnalises :: ([ErroSemantico], [AvisoSemantico], Maybe ExprL) -> ([ErroSemantico], [AvisoSemantico], Maybe ExprL) -> ([ErroSemantico], [AvisoSemantico], Maybe ExprL)
mergeAnalises (e1, a1, expr1) (e2, a2, expr2) = (e1 ++ e2, a1 ++ a2, expr2)

tipoDeExpressao :: [Var] -> Expr -> Type
tipoDeExpressao = inferirTipo

-- Função fictícia para verificar se um tipo é imprimível.
isPrintableType :: Type -> Bool
isPrintableType TVoid = False
isPrintableType _ = True