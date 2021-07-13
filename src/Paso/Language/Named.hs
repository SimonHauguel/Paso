module Paso.Language.Named where

maj_, min_, letter, available :: String
reservedName, reservedOp, otherReserved :: [String]
maj_ = ['A' .. 'Z']
min_ = ['a' .. 'z']
letter = maj_ ++ min_

available =
  [ '<'
  , '>'
  , '.'
  , '!'
  , '@'
  , '#'
  , '$'
  , '%'
  , '^'
  , '&'
  , '*'
  , '~'
  , '?'
  , '+'
  , '-'
  , '_'
  , ':'
  , '|'
  , '='
  ]

reservedName =
  ["if", "match", "define", "fn", "let", "type", "auto", "v", "import"]

reservedOp = ["<~", "~>", "~", "->", "=>", "|", ":=", ":"]

otherReserved = ["#[", "]#", "[", "]", "(", ")", "{", "}", ";", ",", "`"]
