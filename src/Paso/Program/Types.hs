module Paso.Program.Types where

data Builder
  = (:->:) Types Types
  | (:+:)  Types Types
  | (:*:)  Types Types
  | (:~:)  Types
  | Atom

data Types
  = MkTypes
  { typeName  :: String
  , typeValue :: Builder
  }

data Constructor a
  = MkConstructor
  { constructorName :: String
  , constuctorArg :: [a]
  } deriving Show

type TypeLeverConstructor = Constructor Types
