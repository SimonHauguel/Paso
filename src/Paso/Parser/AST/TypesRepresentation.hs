module Paso.Parser.AST.TypesRepresentation where


import           Paso.Parser.AST.Constraint
import           Paso.Program.Context

data Arrow
  = SucredIso String [Arrow]
  | Unique String [Arrow]
  | Fun Arrow Arrow
  deriving Show

data PasoType = PasoType (Context Constraint) Arrow
  deriving Show
data PasoTypeNamed = PasoTypeNamed String PasoType
