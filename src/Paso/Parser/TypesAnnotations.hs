module Paso.Parser.TypesAnnotations where

import           Paso.Parser.ParserData
import           Paso.Language.Tokens                ( Tokens(..) )
import           Paso.Lexer.Stream              ( PTokens
                                                , (===)
                                                )
import qualified Text.Megaparsec               as MG
