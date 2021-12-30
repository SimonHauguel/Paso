{-# LANGUAGE TupleSections #-}
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
  where pureExpr      = ifParse <|> num <|> lambda <|> letBind <|> iden
        contextExpr   = do
          let betBracket = MG.between (tok TK.OpenBracket) (tok TK.CloseBracket)
          allExpr <- betBracket $ MG.sepEndBy1 expr (tok TK.SemiColon)
          pure $ (if length allExpr == 1 then head else Imperatif) allExpr

ifParse :: Parser Expr
ifParse = (tok TK.If $> uncurry If) <*> (MG.try ifMulti <|> ifMatch)

ifMulti :: Parser TupleIf
ifMulti = do
  let subParserCondExpr =
        (:~~>:) <$> exprPattern <*> (tok TK.BigArrowRight *> expr)
  (UIdentifier "true",) . fromList
    <$> (tok TK.Pipe *> MG.sepBy1 subParserCondExpr (tok TK.Pipe))


ifMatch :: Parser TupleIf
ifMatch = do
  let subParserCondExpr =
        (:~~>:) <$> patternParser <*> (tok TK.BigArrowRight *> expr)
  toMatch <- tok TK.Match *> expr
  (toMatch,) . fromList
    <$> (tok TK.Pipe *> MG.sepBy1 subParserCondExpr (tok TK.Pipe))

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

letBind :: Parser Expr
letBind = do
  name <- tok TK.Let *> (tok idenTok <|> tok idenOpTok)
  args <- MG.manyTill (tok idenOpTok <|> tok idenTok) (tok TK.MorseEqual)
  value <- expr
  let unsucredExpr = foldr (Lambda . getName) value args
  pure $ (Let . getName) name unsucredExpr

exprPattern :: Parser MatchConstructor
exprPattern = NonIrrefutable . Left . NotEvaluate <$> expr
