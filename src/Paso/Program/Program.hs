module Paso.Program.Program where


import         Paso.Parser.AST.Function
import         Paso.Parser.AST.Define
import         Paso.Parser.AST.TypesRepresentation

data Program = Program [PasoTypeDec] [Define] [TypedFunction ]
  deriving Show
