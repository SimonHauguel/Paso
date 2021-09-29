module Paso.Parser.TypesAnnotations where

import qualified Text.Megaparsec               as MG
import           Text.Megaparsec                ( (<|>)
                                                , try
                                                )
import           Paso.Parser.ParserData
import           Paso.Program.Context
import qualified Paso.Parser.AST.Constraint    as CN
import           Paso.Parser.AST.TypesRepresentation
import           Paso.Parser.Utils
import           Paso.Language.Tokens
import           Data.Function                  ( on )
import           Data.Set                       ( fromList
                                                , singleton
                                                , empty
                                                )

typeAnnotation :: Parser PasoType -- TODO : Add recursive Arrow type (HOF syntax)
typeAnnotation = (,) <$> constraint <*> subTypeArrow
 where
  typeParser = MG.sepBy1
    (   (Unique . getName <$> tok typeNameTok)
    <|> (SucredIso . getName <$> (tok Iso *> tok typeNameTok))
    <|> parens subTypeArrow
    )
    (tok ArrowRight)

  constraint   = MG.option empty (try $ multiConstraint <* tok BigArrowRight)

  subTypeArrow = foldr1 Fun <$> typeParser


multiConstraint :: Parser (Context CN.Constraint)
multiConstraint = multi <|> single
 where
  subParseMulti = fromList <$> MG.sepBy isoSingleConstraint (tok SemiColon)
  multi         = MG.between (tok OpenBracket) (tok CloseBracket) subParseMulti
  single        = singleton <$> isoSingleConstraint


isoSingleConstraint :: Parser CN.Constraint
isoSingleConstraint = try arrowLeft <|> try arrowRight <|> iso
 where
  allType = tok typeNameTok
  buildConstraint f t = (f `on` getName) <$> allType <*> (tok t *> allType)
  arrowLeft  = buildConstraint (flip CN.Coerce) CoerceLeft
  arrowRight = buildConstraint CN.Coerce CoerceRight
  iso        = buildConstraint CN.Iso Iso

