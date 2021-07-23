module Paso.Parser.IfThenElse where

import qualified Text.Megaparsec as MG
import qualified Paso.Language.Tokens as TK
import Paso.Program.Types
import Paso.Parser.ParserData
import Paso.Parser.Utils
import Paso.Parser.AST.Match
import Paso.Parser.AST.Expr
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty(..))
import Paso.Parser.Expr

mkRes :: String -> Expr -> MatchTo Expr
mkRes a b = NonIrrefutable (MkConstructor a []) :~~>: b

ifParse :: Parser Expr
ifParse = (tok TK.If $> uncurry If) <*> ifPur


ifPur :: Parser TupleIf
ifPur = do
  cond <- expr
  _ <- tok TK.Pipe
  first <- expr
  _ <- tok TK.Pipe
  second <- expr
  pure (cond, mkRes "True" first :| [mkRes "False" second])
