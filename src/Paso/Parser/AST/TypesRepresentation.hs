module Paso.Parser.AST.TypesRepresentation where


import           Paso.Parser.AST.Constraint
import           Paso.Program.Context

data Type
  = SucredIso Type
  | Unique String [Type]
  | Fun Type Type
  | And [Type]
  | Or [(String, PasoType)]
  deriving (Show, Eq)

data PasoType = PasoType (Context Constraint) Type
  deriving Show
data PasoTypeDec = PasoTypeDec String [String] PasoType
  deriving Show


instance Eq PasoType where
  (PasoType _ t) == (PasoType _ t') = t == t'
