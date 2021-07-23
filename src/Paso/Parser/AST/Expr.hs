module Paso.Parser.AST.Expr where

import Paso.Parser.AST.Match
import Data.List.NonEmpty

type TupleIf = (Expr, NonEmpty (MatchTo Expr))

data Expr
  = If Expr (NonEmpty (MatchTo Expr))
  | Let (NonEmpty (MatchTo Expr))
  | TestExpr -- Just to test some parser without be annoying with parse complex expression
             -- Must be deleted in the future
  deriving Show
