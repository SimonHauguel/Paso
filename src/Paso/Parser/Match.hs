module Paso.Parser.Match where


import qualified Text.Megaparsec as MG
import Paso.Language.Tokens
import Control.Monad.Combinators
import Text.Megaparsec ((<|>))
import Paso.Parser.Utils
import Paso.Parser.AST.Expr
import Paso.Parser.AST.Match
import Paso.Parser.ParserData
import Paso.Program.Types


constructorParser :: Parser MatchConstructor
constructorParser = pure $ MkConstructor "Test" []


tupleConstructor :: Parser [MatchConstructor]
tupleConstructor = parens bodyTuple
  where bodyTuple = sepBy2 constructorParser (tok SemiColon)
