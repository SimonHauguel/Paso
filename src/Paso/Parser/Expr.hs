module Paso.Parser.Expr where

import Text.Megaparsec
import Paso.Lexer.Stream
import Paso.Parser.ParserData
import Paso.Parser.AST.Expr
import Data.Functor (($>))


expr :: Parser Expr
expr = satisfy (const True) $> TestExpr
