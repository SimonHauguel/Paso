module Paso.Parser.AST.Define where

import Paso.Parser.AST.Function
import Paso.Parser.AST.TypesRepresentation

data WConstraint = Iso Type Type | Coerce Type Type
  deriving Show
data Define = Define WConstraint [TypedFunction]
  deriving Show
