module Parser.Parser.ParserData where


import qualified Text.Megaparsec               as MG
import           Parser.Lex.Stream
import           Data.Void                      ( Void ) -- TODO Remove to custom error handling
import           Data.Text                      ( pack )
import           Parser.Lex.Lexer


type Parser = MG.Parsec Void LexStream -- TODO Put custom Type Error instead of Void

testPars :: Show a => Parser a -> String -> IO ()
testPars p s = case tokenise "test" (pack s) of
  Right res -> MG.parseTest p res
  Left  err -> putStrLn $ MG.errorBundlePretty err
