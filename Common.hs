module Common where

  -- Comandos interactivos o de archivos
  data Stmt i = Def String i           --  Declarar un nuevo identificador x, let x = t
              | Eval i                 --  Evaluar el término
    deriving (Show)
  
  instance Functor Stmt where
    fmap f (Def s i) = Def s (f i)
    fmap f (Eval i)  = Eval (f i)

  -- Tipos de los nombres
  data Name
     =  Global  String
     |  Quote   Int
    deriving (Show, Eq)

  -- Entornos
  type NameEnv v t = [(Name, (v, t))]

  -- Tipo de los tipos
  data Type = Base
            | Unit
            | Nat
            | Fun Type Type
            | Tup Type Type
            deriving (Show, Eq)
  
  -- Términos con nombres
  data LamTerm  =  LUnit
                |  LZero
                |  LSuc LamTerm
                |  LR LamTerm LamTerm LamTerm
                |  LVar String
                |  Abs String Type LamTerm
                |  App LamTerm LamTerm
                |  LLet String LamTerm LamTerm
                |  LAs LamTerm Type
                |  LTup LamTerm LamTerm
                |  LFst LamTerm
                |  LSnd LamTerm
                deriving (Show, Eq)


  -- Términos localmente sin nombres
  data Term  = TUnit
             | Zero
             | Bound Int
             | Free Name
             | Suc Term
             | R Term Term Term 
             | Term :@: Term
             | Lam Type Term
             | Let Term Term
             | As Term Type
             | TTup Term Term
             | Fst Term
             | Snd Term
          deriving (Show, Eq)

  -- Valores
  data Value = VUnit
             | VZero
             | VSuc Value
             | VLam Type Term 
             | VTup Value Value
            deriving Show

  -- Contextos del tipado
  type Context = [Type]
