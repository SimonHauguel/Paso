module Paso.Parser.Function where

import           Paso.Language.Tokens
import           Control.Monad.Combinators
import           Paso.Parser.Utils
import           Paso.Parser.AST.Expr
import           Paso.Parser.ParserData
import           Paso.Parser.Expr
import           Paso.Parser.AST.Function


-- TODO : Op parser

function :: Parser Function
function = do
  name <- tok idenTok
  args <- manyTill (tok idenTok <|> tok idenOpTok) (tok MorseEqual)
  value <- expr
  let unsucredExpr = foldr (Lambda . getName) value args
  pure $ Function (getName name) unsucredExpr
