module Paso.Parser.AST.Function where

import Paso.Parser.AST.TypesRepresentation
import Paso.Parser.AST.Expr


data Function = Function String Expr
  deriving Show
data TypedFunction = TypedFunction PasoType Function
  deriving Show
