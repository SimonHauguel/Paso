module Paso.Parser.AST.Expr where

import Paso.Parser.AST.Match
import Data.List.NonEmpty

data Expr
  = If Expr (NonEmpty (MatchTo Expr))
  | Some
  deriving Show
