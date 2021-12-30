{-# LANGUAGE OverloadedStrings #-}

module Paso.Lexer.Tokenise
  ( tokenise
  , lexerToken
  )
where

import qualified Text.Megaparsec               as MG
import           Text.Megaparsec                ( (<|>)
                                                , many
                                                , satisfy
                                                , try
                                                )
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer
import qualified Text.Megaparsec.Char.Lexer    as MCL
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Paso.Language.Tokens                ( Tokens(..) )
import           Data.Function                  ( on )
import           Data.Functor                   ( ($>) )
import           Paso.Language.Named
import           Paso.Lexer.Stream

isIn :: [Char] -> Lexer Char
isIn list = satisfy (`elem` list)

tokenise :: String -> Text -> Either Error LexStream
tokenise = MG.parse $ buildPTokensStream lexerToken
 where
  buildPTokensStream :: Lexer Tokens -> Lexer LexStream
  buildPTokensStream lexe = initialize *> MG.manyTill parseAndBuild MG.eof
   where
    parseAndBuild = do
      startPos     <- MG.getSourcePos
      resultLexing <- lexe
      MkPTokens resultLexing startPos <$> MG.getSourcePos

lexerToken :: Lexer Tokens
lexerToken =
  MG.choice
    $   lexeme'
    <$> [litteral, try reser, try real, try relatif, typeName, identifier]

lexeme' :: Lexer a -> Lexer a
lexeme' = lexeme $ MCL.space space1 lineComment_ mLineComment_

initialize :: Lexer ()
initialize = lexeme' $ pure ()

lineComment_ :: Lexer ()
lineComment_ = skipLineComment $ pack ">>"

mLineComment_ :: Lexer ()
mLineComment_ = (skipBlockCommentNested `on` pack) "{>" "<}"

-- Parse Number
maybeSigned :: Num a => Lexer a -> Lexer a
maybeSigned = signed $ pure ()

relatif :: Lexer Tokens
relatif = INT <$> maybeSigned
  (try (char '0' *> (prefixedBinary <|> prefixedHexadecimal)) <|> decimal)

prefixedBinary :: Num a => Lexer a
prefixedBinary = char 'b' *> binary

prefixedHexadecimal :: Num a => Lexer a
prefixedHexadecimal = char 'x' *> hexadecimal

real :: Lexer Tokens
real = FLOAT <$> maybeSigned float

-- Parse Types
typeName :: Lexer Tokens
typeName = do
  fir  <- isIn maj_
  sec <- many (isIn letter)
  pure $ TypeName $ fir : sec


identifier1 :: Lexer Tokens
identifier1 = do
  hea  <- isIn ('_' : min_)
  rest <- many $ isIn $ available ++ letter ++ ['0' .. '9']
  let res = hea : rest
  pure $ case lookup res toTok of
    Just val -> val
    Nothing  -> Iden res
 where
  toTok = zip reservedName
              [If, Match, Define, Fn, Let, Type, Auto, TypeSeparator, Import]

operator :: Lexer Tokens
operator = do
  hea  <- isIn available
  rest <- many (isIn available)
  let res = hea : rest
  pure $ case lookup res toTok of
    Just val -> val
    Nothing  -> IdenOp res
 where
  toTok = zip
    reservedOp
    [ CoerceLeft
    , CoerceRight
    , Iso
    , ArrowRight
    , BigArrowRight
    , Pipe
    , MorseEqual
    , Colon
    ]


identifier :: Lexer Tokens
identifier = identifier1 <|> operator


reser :: Lexer Tokens
reser = MG.choice $ zipWith ($>) (string . pack <$> otherReserved) tok
 where
  tok =
    [ OpenBrace
    , CloseBrace
    , OpenParent
    , CloseParent
    , OpenBracket
    , CloseBracket
    , SemiColon
    , Comma
    , Splice
    ]

litteral :: Lexer Tokens
litteral = STRING <$> (char '"' >> MG.manyTill charLiteral (char '"'))
