module Semantico.Errors where

import Sintatico.Types ( Type, Id,  )

-- Definição da estrutura de erros semânticos
data ErroSemantico
  = IncompatibilidadeTipoString  
  | TiposIncompativeis {tipoEsperado :: Type, tipoEncontrado :: Type}
  | IncompatibilidadeNumeroParametros {nomeFuncao :: Id, parametrosEsperados :: Type, parametrosEncontrados :: Type}
  | IncompatibilidadeTipoParametros {nomeFuncao :: Id, tipoEsperado :: Type, tipoEncontrado :: Type, posicaoParametro :: Type}
  | IncompatibilidadeTipoAtribuicao {nomeVariavel :: Id, tipoEsperado :: Type, tipoEncontrado :: Type}
  | VariavelNaoDeclarada {nomeVariavel :: Id}
  | DeclaracaoMultiplaVariavel {nomeVariavel :: Id}
  | DeclaracaoMultiplaFuncao {nomeFuncao :: Id}
  deriving (Eq, Show)

-- Definição da estrutura de advertências semânticas
data AvisoSemantico
  = CoercaoTipo {tipoOrigem :: Type, tipoDestino :: Type}
  | ConversaoDoubleParaInt
  deriving (Eq, Show)
