module Paso.Parser.AST.Function where

import Paso.Parser.AST.TypesRepresentation
import Paso.Parser.AST.Expr


data Function = Function String Expr
  deriving Show
type TypedFunction = (PasoType, Function)
