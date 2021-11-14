module Paso.Parser.AST.Expr where


import           Data.List.NonEmpty
import           Paso.Program.Types

data Expr
  = If Expr (NonEmpty ToExpr)
  | Let (NonEmpty ToExpr)
  | Function String Expr
  | FunctionCall Expr Expr
  | TestExpr -- TODO Delete
  deriving Show


-- Meta-Match type
data MatchValue -- NOTE : This is not an expression
                -- It's a meta-value
  = Ignore               -- ^ _ pattern [Irrefutable]
  | Variable String      -- ^ a b .. <variable-name> pattern [Irrefutable]
  | Value String         -- ^ TODO {TEMP TO STRING}
                         -- ^ An atomique value [NotIrrefutable]
  | NotEvaluate Expr     -- ^ An expression [NotIrrefutable]
                         -- ^ Like a Value constructor but not evaluate
                         -- ^ Usefull in multi-if pattern
  | Rec MatchConstructor -- ^ When we get a recursive pattern
                         -- ^ [NotIrrefutable]
  deriving Show


data MatchConstructor
  = NonIrrefutable         -- ^ Patterns that can fail
    (Either MatchValue (Constructor MatchValue))
  | Irrefutable MatchValue -- ^ Patterns that can not fail
    deriving Show

data MatchTo a = (:~~>:) MatchConstructor a
  -- ^ "Map" representation
  -- from a match value to a type `a`
  deriving Show

type ToExpr = MatchTo Expr
type TupleIf = (Expr, NonEmpty ToExpr)
