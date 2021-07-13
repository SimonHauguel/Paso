module Paso.Parser.Utils where

import           Paso.Language.Tokens
import qualified Text.Megaparsec               as MG
import           Paso.Parser.ParserData
import           Paso.Lexer.Stream

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
