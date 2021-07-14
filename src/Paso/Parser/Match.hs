module Paso.Parser.Match where


import qualified Text.Megaparsec               as MG
import           Paso.Language.Tokens
import           Control.Monad.Combinators
import           Text.Megaparsec                ( (<|>) )
import           Data.Functor                   ( ($>) )
import           Paso.Parser.Utils
import           Paso.Parser.AST.Expr
import           Paso.Parser.AST.Match
import           Paso.Parser.ParserData
import           Paso.Program.Types


valueConstructorParser :: Parser MatchValue
valueConstructorParser = MG.choice
  [ strictTok (Iden "_") $> Ignore
  , Variable . getName <$> tok idenTok
  , Rec <$> tupleConstructor
  ]


tupleConstructor :: Parser MatchConstructor
tupleConstructor = parens bodyTuple
 where
  bodyTuple =
    MkConstructor "Tuple" <$> sepBy2 valueConstructorParser (tok SemiColon)
