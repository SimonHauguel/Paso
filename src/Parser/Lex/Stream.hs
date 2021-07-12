{-# LANGUAGE FlexibleInstances #-}

module Parser.Lex.Stream
  ( Lexer
  , Error
  , LexStream
  , PTokens(..)
  , (===)
  )
where

import           Data.Foldable                  ( fold )
import           Data.Text                      ( Text )
import           Data.Void                      ( Void )
import           Language.Tokens                ( Tokens(..) )
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
