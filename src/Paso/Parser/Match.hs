module Paso.Parser.Match where


import qualified Text.Megaparsec               as MG
import           Paso.Language.Tokens
import           Control.Monad.Combinators
import           Text.Megaparsec                ( (<|>) )
import           Data.Functor                   ( ($>) )
import           Paso.Parser.Utils
import           Paso.Parser.AST.Expr
import           Paso.Parser.ParserData
import           Paso.Program.Types

-- TODO : Implement exprPattern Correctly
-- TODO : add OpTypePattern parser


valueConstructorParser :: Parser MatchValue
valueConstructorParser = MG.choice
  [ strictTok (Iden "_") $> Ignore
  , Variable . getName <$> tok idenTok
  , Rec <$> number
  , MG.try $ Rec <$> tuplePattern
  , Rec <$> listPattern
  ]

number :: Parser MatchConstructor
number = NonIrrefutable . Left <$> (tok intTok $> Value "Number") -- TODO change the value


tuplePattern :: Parser MatchConstructor
tuplePattern = parens bodyTuple
 where
  bodyTuple =
    NonIrrefutable . Right . MkConstructor "Tuple" <$> sepBy2 valueConstructorParser (tok Comma)

listPattern :: Parser MatchConstructor
listPattern = between (tok OpenBrace) (tok CloseBrace) bodyList
 where
  bodyList =
    NonIrrefutable . Right . MkConstructor "List" <$> sepEndBy valueConstructorParser (tok Comma)

typePattern :: Parser MatchConstructor
typePattern = do
  wName <- tok typeNameTok
  NonIrrefutable . Right . MkConstructor (getName wName) <$> many valueConstructorParser

patternParser :: Parser MatchConstructor
patternParser =
  MG.choice [MG.try tuplePattern, typePattern, listPattern,  number]
  <|> Irrefutable <$> valueConstructorParser
  <|> parens patternParser

exprPattern :: Parser MatchConstructor
exprPattern = strictTok (Iden "_") $> Irrefutable Ignore <|> (strictTok (Iden "cond") $> NonIrrefutable (Left $ NotEvaluate TestExpr))
