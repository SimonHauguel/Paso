{-# LANGUAGE FlexibleInstances #-}

module Paso.Lexer.Stream
  ( Lexer
  , Error
  , LexStream
  , PTokens(..)
  , (===)
  , (#==)
  )
where

import           Data.Foldable                  ( fold )
import           Data.Text                      ( Text )
import           Data.Void                      ( Void )
import           Paso.Language.Tokens           ( Tokens(..) )
import qualified Text.Megaparsec               as MG
import           Text.Megaparsec.Pos
import           Text.Megaparsec.Stream         ( TraversableStream
                                                , VisualStream
                                                )

type Lexer = MG.Parsec Void Text

type Error = MG.ParseErrorBundle Text Void

type LexStream = [PTokens]

data PTokens =
  MkPTokens
    { value         :: Tokens
    , positionStart :: SourcePos
    , positionEnd   :: SourcePos
    }
  deriving (Show, Ord)

instance Eq PTokens where
  MkPTokens { value = a } == MkPTokens { value = b } = a == b

instance VisualStream LexStream where
  showTokens _ val =
    fold $ (\MkPTokens { value = a } -> "( " <> show a <> " )") <$> val

instance TraversableStream LexStream where
  reachOffsetNoLine _ = id

infix 4 ===

(===) :: Tokens -> PTokens -> Bool
tok === MkPTokens { value = tok' } = tok == tok'

(#==) :: Tokens -> PTokens -> Bool
tok #== MkPTokens {value = tok' } = case (tok, tok') of
  (Iden a, Iden b)         -> a == b
  (IdenOp a, IdenOp b)     -> a == b
  (TypeName a, TypeName b) -> a == b
  (INT a, INT b)           -> a == b
  (FLOAT a, FLOAT b)       -> a == b
  (STRING a, STRING b)     -> a == b
  (a, b)                   -> a == b
