module Paso.Parser.Lambda where


import qualified Text.Megaparsec               as MG
import           Text.Megaparsec                ( (<|>) )
import qualified Paso.Language.Tokens          as TK
import           Paso.Parser.AST.Expr
import           Paso.Parser.ParserData
import           Paso.Parser.Utils
import           Paso.Parser.Expr

lambda :: Parser Expr
lambda = do
  _     <- tok TK.Fn
  arg   <- MG.many (tok idenTok <|> tok idenOpTok)
  _     <- tok TK.BigArrowRight
  value <- expr
  pure $ foldr Function value (getName <$> arg)
