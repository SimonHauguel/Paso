module Program.Types where

data Builder
  = (:->:) Types Types
  | (:+:)  Types Types
  | (:*:)  Types Types
  | (:~:)  Types
  | Atom

data Types
  = MkTypes
  { name  :: String
  , value :: Builder
  }
