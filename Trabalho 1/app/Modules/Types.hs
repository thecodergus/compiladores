module Types where

-- Tipo para representar identificadores.
type Id = String

-- Tipos de dados suportados pela linguagem: Double, Int, String e Void.
data Type
  = TDouble
  | TInt
  | TString
  | TVoid
  deriving (Show)

-- Representação de constantes na linguagem: Double, Int e String.
data TCons
  = CDouble Double
  | CInt Integer
  deriving (Show)

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
  deriving (Show)

-- Representação das expressões relacionais na linguagem.
data ExprR
  = Expr :==: Expr
  | Expr :/=: Expr
  | Expr :<: Expr
  | Expr :>: Expr
  | Expr :<=: Expr
  | Expr :>=: Expr
  deriving (Show)

-- Representação das expressões lógicas na linguagem.
data ExprL
  = ExprL :&: ExprL
  | ExprL :|: ExprL
  | Not ExprL
  | Rel ExprR
  deriving (Show)

-- Representação de variáveis na linguagem (identificador e tipo).
data Var = Id :#: Type deriving (Show)

-- Representação de funções na linguagem (identificador, lista de parâmetros e tipo de retorno).
data Funcao = Id :->: ([Var], Type) deriving (Show)

-- Representação do programa na linguagem (lista de funções e bloco principal).
data Programa = Prog [Funcao] [(Id, [Var], Bloco)] [Var] Bloco deriving (Show)

-- Representação de um bloco de comandos na linguagem.
type Bloco = [Comando]

-- Representação dos comandos na linguagem (if, while, atribuição, leitura, impressão e retorno).
data Comando
  = If ExprL Bloco Bloco
  | While ExprL Bloco
  | Atrib Id Expr
  | Leitura Id
  | Imp Expr
  | Ret (Maybe Expr)
  | Proc Id [Expr]
  deriving (Show)
