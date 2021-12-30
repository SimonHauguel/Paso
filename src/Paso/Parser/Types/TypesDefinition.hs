module Paso.Parser.Types.TypesDefinition
  ( typeDefinition
  )
where

import qualified Text.Megaparsec as MG
import           Text.Megaparsec ( (<|>) )
import           Paso.Parser.Types.TypesAnnotations
import           Paso.Parser.AST.TypesRepresentation
import qualified Paso.Language.Tokens          as TK
import           Paso.Parser.Utils
import           Paso.Parser.ParserData


typeDefinition :: Parser PasoTypeDec
typeDefinition = do
  constr <- tok TK.Type *> constraint
  name <- tok typeNameTok
  args <- MG.many (tok typeNameTok) <* tok TK.MorseEqual
  allValues <- MG.between (tok TK.OpenBracket) (tok TK.CloseBracket)
             $ MG.sepBy subType (tok TK.TypeSeparator)
  pure $ (PasoTypeDec . getName) name (getName <$> args) (PasoType constr (Or allValues))
  where subType = do
          cons    <- constraint
          subName <- tok idenTok <|> tok idenOpTok
          allSlot <- MG.sepBy subTypeArrow (tok TK.SemiColon)
          pure (getName subName, PasoType cons (And allSlot))
