module Paso.Parser.TypesAnnotations
  ( typeAnnotation
  )
where

import qualified Text.Megaparsec               as MG
import           Text.Megaparsec                ( (<|>)
                                                , try
                                                )
import           Paso.Parser.ParserData
import           Paso.Program.Types
import           Paso.Program.Context
import qualified Paso.Parser.AST.Constraint    as CN
import           Paso.Parser.Utils
import           Paso.Language.Tokens
import           Data.Bifunctor                 ( bimap )
import           Data.Function                  ( on )

typeAnnotation :: Parser a
typeAnnotation = undefined

isoSingleConstraint :: Parser CN.Constraint
isoSingleConstraint = try arrowLeft <|> try arrowRight <|> iso
 where
  allType = tok typeNameTok
  buildConstraint f t = (f `on` getName) <$> allType <*> (tok t *> allType)
  arrowLeft = buildConstraint (flip CN.Coerce) CoerceLeft
  arrowRight = buildConstraint CN.Coerce CoerceRight
  iso = buildConstraint CN.Iso Iso
