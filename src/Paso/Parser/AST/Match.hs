module Paso.Parser.AST.Match where

import           Paso.Program.Types

data MatchValue
  = Ignore
  | Variable String
  | Value String -- Temp
  | Rec MatchConstructor
  deriving Show

data MatchConstructor = NonIrrefutable (Either MatchValue (Constructor MatchValue))
                      | Irrefutable MatchValue
                      deriving Show

data MatchTo a = (:~~>:) MatchConstructor a
  deriving Show
