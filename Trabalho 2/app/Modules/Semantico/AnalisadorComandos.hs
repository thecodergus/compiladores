{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Semantico.AnalisadorComandos where
import Sintatico.Types (Var ((:#:)), Bloco, Comando (Atrib), Id, Expr (Chamada), Type (TDouble, TInt))
import Semantico.ErrosSemantico (ErroSemantico (IncompatibilidadeTipoAtribuicao, VariavelNaoDeclarada))
import Semantico.AvisosSemantico (AvisoSemantico (CoercaoTipo, ConversaoDoubleParaInt))
import Data.Foldable (find)
import Semantico.AnalisadorVariaveis (inferirTipo)

-- import 


-- Função que analisa um bloco de comandos
analisarComandos :: [Var] -> Bloco -> ([ErroSemantico], [AvisoSemantico])
analisarComandos vars = foldl (analisarComando vars) ([], [])

-- Função que analisa um comando individual
analisarComando :: [Var] -> ([ErroSemantico], [AvisoSemantico]) -> Comando -> ([ErroSemantico], [AvisoSemantico])
analisarComando vars (erros, avisos) comando =
  case comando of
    Atrib id expr ->
      let (errs, avs) = analisarAtribuicao vars id expr
       in (erros ++ errs, avisos ++ avs)