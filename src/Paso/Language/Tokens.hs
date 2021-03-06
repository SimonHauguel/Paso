{-# LANGUAGE FlexibleInstances #-}

module Paso.Language.Tokens where

type Name = String

data Tokens
  = IdenOp Name
  | Iden Name
  | INT Integer
  | FLOAT Float
  | TypeName Name
  | STRING String
  | If              -- ^ if
  | Match           -- ^ match
  | Define          -- ^ define
  | Fn              -- ^ fn
  | Let             -- ^ let
  | Type            -- ^ type
  | Auto            -- ^ auto
  | Import          -- ^ import
  | ArrowRight      -- ^ ->
  | BigArrowRight   -- ^ =>
  | CoerceLeft      -- ^ <~
  | CoerceRight     -- ^ ~>
  | Iso             -- ^ ~
  | OpenBrace       -- ^ [
  | CloseBrace      -- ^ ]
  | OpenParent      -- ^ (
  | CloseParent     -- ^ )
  | OpenBracket     -- ^ {
  | CloseBracket    -- ^ }
  | SemiColon       -- ^ ;
  | Comma           -- ^ ,
  | MorseEqual      -- ^ :=
  | Pipe            -- ^ |
  | Colon           -- ^ :
  | TypeSeparator   -- ^ v
  | Splice          -- ^ `
  deriving (Show, Ord)


instance Eq Tokens where
  STRING   _ == STRING   _ = True
  TypeName _ == TypeName _ = True
  FLOAT    _ == FLOAT    _ = True
  INT      _ == INT      _ = True
  Iden     _ == Iden     _ = True
  IdenOp   _ == IdenOp   _ = True
  a          == b          = show a == show b
