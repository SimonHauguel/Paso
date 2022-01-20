module Paso.Parser.Program where

import qualified Text.Megaparsec                   as MG
import           Paso.Parser.ParserData
import           Paso.Program.Program              ( Program(..) )
import           Paso.Parser.Function              (typedFunction)
import           Paso.Parser.Define                (defineConstraint)
import           Paso.Parser.Types.TypesDefinition (typeDefinition)

program :: Parser Program
program =
  Program
    <$> MG.many typeDefinition
    <*> MG.many defineConstraint
    <*> MG.many typedFunction
