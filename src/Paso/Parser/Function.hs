module Paso.Parser.Function where

import qualified Paso.Language.Tokens               as TK
import           Control.Monad.Combinators
import           Paso.Parser.AST.Function
import           Paso.Parser.AST.Expr
import           Paso.Parser.Utils
import           Paso.Parser.ParserData
import           Paso.Parser.Expr
import           Paso.Parser.Types.TypesAnnotations
import           Control.Monad                      (guard)

function :: Parser Function
function = do
  name <- tok idenTok <|> tok idenOpTok
  args <- manyTill (tok idenTok <|> tok idenOpTok) (tok TK.MorseEqual)
  value <- expr <* tok TK.SemiColon
  let unsucredExpr = foldr (Lambda . getName) value args
  pure $ Function (getName name) unsucredExpr

typedFunction :: Parser TypedFunction
typedFunction = do
  fstName <- (tok idenTok <|> tok idenOpTok) <* tok TK.Colon
  typeF <- typeAnnotation
  fn <- function
  case fn of
    Function sndName _ -> do
      guard (getName fstName == sndName)
      pure $ TypedFunction typeF fn
