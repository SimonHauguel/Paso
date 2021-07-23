module Paso.Parser.IfThenElse where

import qualified Text.Megaparsec               as MG
import qualified Paso.Language.Tokens          as TK
import           Paso.Program.Types
import           Paso.Parser.ParserData
import           Paso.Parser.Utils
import           Paso.Parser.AST.Match
import           Paso.Parser.AST.Expr
import           Data.Functor                   ( ($>) )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Paso.Parser.Expr


ifParse :: Parser Expr
ifParse = (tok TK.If $> uncurry If) <*> ifPur

ifPur :: Parser TupleIf -- if a | b | c
ifPur = do
  cond   <- expr
  first  <- tok TK.Pipe *> expr
  second <- tok TK.Pipe *> expr
  pure
    ( cond
    , NonIrrefutable (Left $ Value "True")
    :~~>: first
    :|    [Irrefutable Ignore :~~>: second]
    )

ifMulti :: Parser TupleIf -- if | cond => a | cond => b
ifMulti = do
  _        <- tok TK.Pipe
  listeRes <- MG.sepBy1 expr (tok TK.Pipe)
  undefined
