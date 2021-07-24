module Paso.Parser.AST.Constraint where


data Constraint
  = Coerce String String
  | Iso String String
  deriving (Show, Eq, Ord)
