module Paso.Parser.AST.Expr where

import Paso.Parser.AST.Match
import Data.List.NonEmpty

data Expr
  = If Expr (NonEmpty (MatchTo Expr))
  | Let (Either MatchConstructor String) Expr
  | Some -- Just to test some parser without be annoying with parse some complex expression
         -- Must be deleted in the future
  deriving Show
