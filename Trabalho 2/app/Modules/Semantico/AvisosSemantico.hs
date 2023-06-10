module Semantico.AvisosSemantico where

import Sintatico.Types ( Type  )

-- Definição da estrutura de advertências semânticas
data AvisoSemantico
  = CoercaoTipo {tipoOrigem :: Type, tipoDestino :: Type}
  | ConversaoDoubleParaInt
  | ConversaoIntParaDouble
  deriving (Eq, Show)