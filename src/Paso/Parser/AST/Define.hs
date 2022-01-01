module Paso.Parser.AST.Define where

import Paso.Parser.AST.Function
import Paso.Parser.AST.TypesRepresentation

data WConstraint = ISO Type Type | Coerce Type Type
data Define = Define WConstraint [Function]
