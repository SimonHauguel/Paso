module Paso.Parser.Types.TypesAnnotations where

import qualified Text.Megaparsec               as MG
import           Text.Megaparsec                ( (<|>)
                                                , try
                                                , many
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

typeAnnotation :: Parser PasoType
typeAnnotation = PasoType <$> constraint <*> subTypeArrow

subTypeArrow :: Parser Type
subTypeArrow = foldr1 Fun <$> typeParser
  where
  typeParser   = (typeSubParser `MG.sepBy1` tok ArrowRight) <|> parens typeParser
  typeSubParser = MG.choice
    [ Unique . getName <$> tok typeNameTok
                       <*> try
                          (many $ parens subTypeArrow
                          <|> (`Unique` []) . getName <$> tok typeNameTok)
    , SucredIso <$> (tok Iso *> (parens subTypeArrow <|> typeSubParser))
    , parens subTypeArrow
    ]


constraint :: Parser (Context CN.Constraint)
constraint = MG.option empty (try $ multiConstraint <* tok BigArrowRight)

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
