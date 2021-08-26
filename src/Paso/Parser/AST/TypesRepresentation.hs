module Paso.Parser.AST.TypesRepresentation where


import Paso.Parser.AST.Constraint
import Paso.Program.Context

data Arrow = SucredIso String | Unique String | Fun Arrow Arrow
  deriving Show

type PasoType = (Context Constraint, Arrow)
