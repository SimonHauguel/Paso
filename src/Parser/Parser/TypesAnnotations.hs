module Parser.Parser.TypesAnnotations where

import           Parser.Parser.ParserData
import           Language.Tokens                ( Tokens(..) )
import           Parser.Lex.Stream              ( PTokens
                                                , (===)
                                                )
import qualified Text.Megaparsec               as MG
