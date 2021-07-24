module Paso.Parser.IfThenElse where

import qualified Text.Megaparsec               as MG
import qualified Paso.Language.Tokens          as TK
import           Paso.Parser.ParserData
import           Paso.Parser.Utils
import           Paso.Parser.AST.Expr
import           Data.Functor                   ( ($>) )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Paso.Parser.Expr

mkIf :: [Either Expr MatchValue] -> TupleIf
mkIf = undefined

ifParse :: Parser Expr
ifParse = (tok TK.If $> uncurry If) <*> ifPur


-- An if a | b | c
-- compile to this structure :
-- if match a | True => b
--            | _ => c
ifPur :: Parser TupleIf
ifPur = do
  cond   <- expr
  first  <- tok TK.Pipe *> expr
  second <- tok TK.Pipe *> expr
  pure
    ( cond
    , (NonIrrefutable (Left $ Value "True") :~~>: first)
      :| [Irrefutable Ignore :~~>: second]
    ) -- TODO change \"True\" value


-- An if | cond1 => a
--       | cond2 => b
--       | _     => c
-- compile to this structure :
-- if cond1 | a | (if cond2 | b | c)
ifMulti :: Parser TupleIf
ifMulti = do
  _        <- tok TK.Pipe
  listeRes <- MG.sepBy1 subParserCondExpr (tok TK.Pipe)
  undefined
    where subParserCondExpr = do
            first  <- expr
            _      <- tok TK.BigArrowRight
            second <- expr
            pure (first, second)
