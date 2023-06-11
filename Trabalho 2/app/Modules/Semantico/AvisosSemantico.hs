module Semantico.AvisosSemantico where

import Sintatico.Types ( Type, Expr  )

-- Definição da estrutura de advertências semânticas
data AvisoSemantico
  = CoercaoTipo {tipoOrigem :: Type, tipoDestino :: Type}
  | ConversaoAutomatica {tipoOrigem :: Type, tipoDestino :: Type}
  | ConversaoAutomaticaExpr { expressao :: Expr, tipoOrigem :: Type, tipoDestino :: Type }
  deriving (Eq, Show)