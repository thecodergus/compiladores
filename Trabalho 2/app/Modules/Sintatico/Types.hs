module Sintatico.Types where
{-
O módulo Types é usado para definir os tipos de dados que serão usados na representação interna de uma linguagem de programação.
-}

-- Tipo para representar identificadores.
type Id = String

-- Tipos de dados suportados pela linguagem: Double, Int, String e Void.
data Type
  = TDouble
  | TInt
  | TString
  | TVoid
  deriving (Eq, Show)

-- Representação de constantes na linguagem: Double, Int e String.
data TCons
  = CDouble Double
  | CInt Integer
  deriving (Eq, Show)

-- Representação das expressões aritméticas na linguagem.
data Expr
  = Expr :+: Expr
  | Expr :-: Expr
  | Expr :*: Expr
  | Expr :/: Expr
  | Neg Expr
  | Const TCons
  | IdVar String
  | Chamada Id [Expr]
  | Lit String
  | ConverterPara Type Expr
  deriving (Eq, Show)

-- Representação das expressões relacionais na linguagem.
data ExprR
  = Expr :==: Expr
  | Expr :/=: Expr
  | Expr :<: Expr
  | Expr :>: Expr
  | Expr :<=: Expr
  | Expr :>=: Expr
  deriving (Eq, Show)

-- Representação das expressões lógicas na linguagem.
data ExprL
  = ExprL :&: ExprL
  | ExprL :|: ExprL
  | Not ExprL
  | Rel ExprR
  deriving (Eq, Show)

-- Representação de variáveis na linguagem (identificador e tipo).
data Var = Id :#: Type deriving (Eq, Show)

-- Representação de funções na linguagem (identificador, lista de parâmetros e tipo de retorno).
data Funcao = Id :->: ([Var], Type) deriving (Eq, Show)
type FuncaoBloco = (Id, [Var], Bloco) -- Representação de funções com bloco de comandos

-- Representação do programa na linguagem (lista de funções e bloco principal).
-- Modifiquei para incluir a lista de variáveis locais de cada função
data Programa = Prog [Funcao] [FuncaoBloco] [Var] Bloco deriving (Eq, Show)

-- Representação de um bloco de comandos na linguagem.
type Bloco = [Comando]

-- Representação dos comandos na linguagem (if, while, atribuição, leitura, impressão, retorno e chamada de função).
data Comando
  = If ExprL Bloco Bloco
  | While ExprL Bloco
  | Atrib Id Expr
  | Leitura Id
  | Imp Expr
  | Ret (Maybe Expr)
  | Proc Id [Expr]
  deriving (Eq, Show)