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
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Paso.Parser.Expr
import           Paso.Parser.Match

ifParse :: Parser Expr
ifParse =
  (tok TK.If $> uncurry If) <*> (MG.try ifPur <|> MG.try ifMulti <|> ifMatch)


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
-- if match True | cond1 => a
--               | cond2 => b
--               | _     => c
ifMulti :: Parser TupleIf
ifMulti = do
  listeRes <- tok TK.Pipe *> MG.sepBy1 subParserCondExpr (tok TK.Pipe)
  pure
    ( TestExpr
    , case listeRes of
      (x : xs) -> x :| xs
      _        -> undefined
    )
  -- TODO Edit TestExpr value

 where
  subParserCondExpr =
    (:~~>:) <$> exprPattern <*> (tok TK.BigArrowRight *> expr)

ifMatch :: Parser TupleIf
ifMatch = do
  toMatch  <- tok TK.Match *> expr
  listeRes <- tok TK.Pipe *> MG.sepBy1 subParserCondExpr (tok TK.Pipe)
  pure
    ( toMatch
    , case listeRes of
      (x : xs) -> x :| xs
      _        -> undefined
    )
  -- TODO Edit TestExpr value

 where
  subParserCondExpr =
    (:~~>:) <$> patternParser <*> (tok TK.BigArrowRight *> expr)
