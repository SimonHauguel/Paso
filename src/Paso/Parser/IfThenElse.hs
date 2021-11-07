module Paso.Parser.IfThenElse
  ( ifParse
  )
where

import qualified Text.Megaparsec               as MG
import           Text.Megaparsec                ( (<|>) )
import qualified Paso.Language.Tokens          as TK
import           Paso.Parser.ParserData
import           Paso.Parser.Utils
import           Paso.Parser.AST.Expr
import           Data.Functor                   ( ($>) )
import           Data.List.NonEmpty             ( fromList )
import           Paso.Parser.Expr
import           Paso.Parser.Match

ifParse :: Parser Expr
ifParse = (tok TK.If $> uncurry If) <*> (MG.try ifMulti <|> ifMatch)


-- An if | cond1 => a
--       | cond2 => b
--       | _     => c
-- compile to this structure :
-- if match True | cond1 => a
--               | cond2 => b
--               | _     => c
ifMulti :: Parser TupleIf
ifMulti = do
  let subParserCondExpr =
        (:~~>:) <$> exprPattern <*> (tok TK.BigArrowRight *> expr)
  listRes <- tok TK.Pipe *> MG.sepBy1 subParserCondExpr (tok TK.Pipe)
  pure (TestExpr, fromList listRes)
  -- TODO Edit TestExpr value



ifMatch :: Parser TupleIf
ifMatch = do
  let subParserCondExpr =
        (:~~>:) <$> patternParser <*> (tok TK.BigArrowRight *> expr)
  toMatch <- tok TK.Match *> expr
  listRes <- tok TK.Pipe *> MG.sepBy1 subParserCondExpr (tok TK.Pipe)
  pure (toMatch, fromList listRes)
  -- TODO Edit TestExpr value
