module Paso.Parser.Utils where

import           Paso.Language.Tokens
import qualified Text.Megaparsec               as MG
import           Paso.Parser.ParserData
import           Paso.Lexer.Stream
import           Control.Monad.Combinators



stringTok :: Tokens
stringTok = STRING mempty

idenTok :: Tokens
idenTok = Iden mempty

idenOpTok :: Tokens
idenOpTok = IdenOp mempty

typeVarTok :: Tokens
typeVarTok = TypeVar mempty

typeNameTok :: Tokens
typeNameTok = TypeName mempty

tok :: Tokens -> Parser PTokens
tok t = MG.satisfy (t ===)

parens :: Parser a -> Parser a
parens = MG.between (tok OpenParent) (tok CloseParent)

sepBy2 :: Parser a -> Parser sep -> Parser [a]
sepBy2 p sep = do
  first <- p
  _ <- sep
  (first :) <$> sepBy1 p sep
