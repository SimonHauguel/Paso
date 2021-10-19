module Paso.Parser.AST.TypesRepresentation where


import           Paso.Parser.AST.Constraint
import           Paso.Program.Context

data Arrow = SucredIso String | Unique String | Fun Arrow Arrow
  deriving Show

data  PasoType = PasoType (Context Constraint) Arrow
data PasoTypeNamed = PasoTypeNamed String PasoType
