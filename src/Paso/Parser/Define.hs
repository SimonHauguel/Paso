{-# LANGUAGE MultiWayIf #-}

module Paso.Parser.Define where

import qualified Text.Megaparsec        as MG
import           Text.Megaparsec        ( (<|>) )
import qualified Paso.Language.Tokens   as TK
import           Paso.Lexer.Stream
import           Paso.Parser.ParserData
import           Paso.Parser.AST.Define
import           Paso.Parser.Function
import           Paso.Parser.Types.TypesAnnotations
import           Paso.Parser.Utils


defineConstraint :: Parser Define
defineConstraint = do
  firstType   <- tok TK.Define *> subTypeArrow
  wCons       <- tok TK.CoerceLeft <|> tok TK.CoerceRight <|> tok TK.Iso
  sndType     <- subTypeArrow <* tok TK.MorseEqual
  allFunction <- MG.between (tok TK.OpenBracket) (tok TK.CloseBracket) $ MG.some  typedFunction
  pure $ if | (TK.CoerceLeft === wCons)  -> Define (Coerce sndType firstType) allFunction
            | (TK.CoerceRight === wCons) -> Define (Coerce firstType sndType) allFunction
            | otherwise                  -> Define (Iso    firstType sndType) allFunction
