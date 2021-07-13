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

test :: Parser Expr
test = tok idenTok $> Some -- Use only to test the parser

ifPur :: Parser Expr
ifPur = do
  _ <- tok TK.If
  cond <- test
  _ <- tok TK.Pipe
  first <- parseExpr
  _ <- tok TK.Pipe
  If cond . mkConstruct first <$> parseExpr
    where parseExpr = MG.between (tok TK.OpenBracket) (tok TK.CloseBracket) test
          mkConstruct first second = MkConstructor "True" [] :~~>: first :| [MkConstructor "False" [] :~~>: second]
