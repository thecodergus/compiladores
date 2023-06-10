module Semantico.ErrosSemantico where

import Sintatico.Types ( Type, Id,  )


-- Definição da estrutura de erros semânticos
data ErroSemantico
  = IncompatibilidadeTipoString
  | TiposIncompativeis {tipoEsperado :: Type, tipoEncontrado :: Type}
  | IncompatibilidadeNumeroParametros {nomeFuncao :: Id, parametrosEsperados :: Int, parametrosEncontrados :: Int}
  | IncompatibilidadeTipoParametros {nomeFuncao :: Id, tipoEsperado :: Type, tipoEncontrado :: Type, posicaoParametro :: Int}
  | IncompatibilidadeTipoAtribuicao {nomeVariavel :: Id, tipoEsperado :: Type, tipoEncontrado :: Type}
  | VariavelNaoDeclarada {nomeVariavel :: Id}
  | DeclaracaoMultiplaVariavel {nomeVariavel :: Id}
  | DeclaracaoMultiplaFuncao {nomeFuncao :: Id}
  | NumeroParametrosIncorreto {nomeFuncao :: Id}
  | TiposParametrosIncompativel {nomeFuncao :: Id}
  | ChamadaFuncaoNaoDeclarada {nomeFuncao :: Id}
  | FuncaoMultiplamenteDeclarada {nomeFuncao :: Id}
  deriving (Eq, Show)
