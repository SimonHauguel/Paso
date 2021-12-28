module Paso.Parser.Utils where

import           Paso.Language.Tokens
import qualified Text.Megaparsec               as MG
import           Paso.Parser.ParserData
import           Control.Monad.Combinators
import           Paso.Lexer.Stream


stringTok :: Tokens
stringTok = STRING mempty

idenTok :: Tokens
idenTok = Iden mempty

idenOpTok :: Tokens
idenOpTok = IdenOp mempty

typeNameTok :: Tokens
typeNameTok = TypeName mempty

intTok :: Tokens
intTok = INT 0

floatTok :: Tokens
floatTok = FLOAT 0.0

tok :: Tokens -> Parser PTokens
tok t = MG.satisfy (t ===)

strictTok :: Tokens -> Parser PTokens
strictTok t = MG.satisfy (t #==)

parens :: Parser a -> Parser a
parens = MG.between (tok OpenParent) (tok CloseParent)

sepBy2 :: Parser a -> Parser sep -> Parser [a]
sepBy2 p sep = do
  first <- p
  _ <- sep
  (first :) <$> sepBy1 p sep

getName :: PTokens -> String
getName MkPTokens {value = a} = case a of
  Iden     x -> x
  IdenOp   x -> x
  TypeName x -> x
  STRING   x -> x
  _          -> mempty

extInt :: PTokens -> Integer
extInt MkPTokens {value = a} = case a of
    INT x -> x
    _     -> 0

extFloat :: PTokens -> Float
extFloat MkPTokens {value = a} = case a of
    FLOAT x -> x
    _       -> 0.0
