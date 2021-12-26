module Paso.Parser.AST.TypesRepresentation where


import           Paso.Parser.AST.Constraint
import           Paso.Program.Context

data Arrow
  = SucredIso Arrow
  | Unique String [Arrow]
  | Fun Arrow Arrow
  deriving (Show, Eq)

data PasoType = PasoType (Context Constraint) Arrow
  deriving Show
data PasoTypeNamed = PasoTypeNamed String PasoType


instance Eq PasoType where
  (PasoType _ t) == (PasoType _ t') = t == t'
