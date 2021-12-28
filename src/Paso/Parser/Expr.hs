module Paso.Parser.Expr where


import qualified Text.Megaparsec                as MG
import           Text.Megaparsec                ( (<|>) )
import qualified Paso.Language.Tokens           as TK
import           Paso.Parser.ParserData
import           Paso.Parser.Utils
import           Paso.Parser.AST.Expr
import           Data.Functor                   ( ($>) )
import           Data.List.NonEmpty             ( fromList )
import           Paso.Parser.Match


expr :: Parser Expr
expr = MG.try contextExpr <|> pureExpr <|> parens expr
  where pureExpr      = ifParse <|> num <|> lambda <|> iden
        contextExpr   = MG.between (tok TK.OpenBracket) (tok TK.CloseBracket)
                        $ Imperatif <$> sepEndBy2 expr (tok TK.SemiColon)
        sepEndBy2 p s = (:) <$> (p <* s) <*> MG.sepEndBy1 p s

ifParse :: Parser Expr
ifParse = (tok TK.If $> uncurry If) <*> (MG.try ifMulti <|> ifMatch)

ifMulti :: Parser TupleIf
ifMulti = do
  let subParserCondExpr =
        (:~~>:) <$> exprPattern <*> (tok TK.BigArrowRight *> expr)
  listRes <- tok TK.Pipe *> MG.sepBy1 subParserCondExpr (tok TK.Pipe)
  pure (UIdentifier "true", fromList listRes)


ifMatch :: Parser TupleIf
ifMatch = do
  let subParserCondExpr =
        (:~~>:) <$> patternParser <*> (tok TK.BigArrowRight *> expr)
  toMatch <- tok TK.Match *> expr
  listRes <- tok TK.Pipe *> MG.sepBy1 subParserCondExpr (tok TK.Pipe)
  pure (toMatch, fromList listRes)

lambda :: Parser Expr
lambda = do
  _     <- tok TK.Fn
  arg   <- MG.many (tok idenTok <|> tok idenOpTok)
  _     <- tok TK.BigArrowRight
  value <- expr
  pure $ foldr Lambda value (getName <$> arg)

num :: Parser Expr
num = numberI <|> numberF
  where
    numberI = NumberI . extInt   <$> tok intTok
    numberF = NumberF . extFloat <$> tok floatTok

iden :: Parser Expr
iden = UIdentifier . getName <$> (tok idenOpTok <|> tok idenTok)

exprPattern :: Parser MatchConstructor
exprPattern = NonIrrefutable . Left . NotEvaluate <$> expr
