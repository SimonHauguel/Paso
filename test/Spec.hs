import           Test.Hspec.Megaparsec
import           Test.Hspec
import           Parser.Lex.Lexer
import           Parser.Lex.Stream
import           Language.Tokens
import           Data.Text                      ( pack )
import           Text.Megaparsec         hiding ( Tokens )

testLexer :: String -> Either Error [Tokens]
testLexer = parse (manyTill lexerToken eof) mempty . pack

main :: IO ()
main = hspec $ describe "Lexer Suite Test" $ do
  it "Nothing To Lexe" $ testLexer mempty `shouldParse` []
  it "Reserved Names"
    $             testLexer
                    "if match define fn let type auto -> => <~ ~> ~ #[ ]# [  ] (  ) {  } ; , := | : v `"

    `shouldParse` [ If
                  , Match
                  , Define
                  , Fn
                  , Let
                  , Type
                  , Auto
                  , ArrowRight
                  , BigArrowRight
                  , CoerceLeft
                  , CoerceRight
                  , Iso
                  , OpenMacroBlock
                  , CloseMacroBlock
                  , OpenBrace
                  , CloseBrace
                  , OpenParent
                  , CloseParent
                  , OpenBracket
                  , CloseBracket
                  , SemiColon
                  , Comma
                  , MorseEqual
                  , Pipe
                  , Colon
                  , TypeSeparator
                  , Splice
                  ]
  it "Operators"
    $             testLexer "+ +_ <~~ ==> _ +_"
    `shouldParse` [ IdenOp "+"
                  , IdenOp "+_"
                  , IdenOp "<~~"
                  , IdenOp "==>"
                  , Iden "_"
                  , IdenOp "+_"
                  ]
  it "Identifier"
    $             testLexer "a+ a +a"
    `shouldParse` [Iden "a+", Iden "a", IdenOp "+", Iden "a"]
  it "Numbers (Float And Int)"
    $             testLexer "1 1.0"
    `shouldParse` [INT 1, FLOAT 1.0]
  it "String and Escape"
    $             testLexer "\"a\\\"b\n\t\""
    `shouldParse` [String "a\"b\n\t"]
  it "In a tricky case (entire program)"
    $             testLexer
                    "type Either A B :={Left A>> Ignore This Message\nv Right B{>{><}Also Ignore This Message<}};type A :|:B:={It A v This B};define A :|:B~Either A B :={from :A :|:B ->Either A B;from arg :={let a+ :=a+ +a;a+ + +a- a};to :Either A B-> A :|:B;to arg :={if arg |some =>test;};"
    `shouldParse` [ Type
                  , TypeName "Either"
                  , TypeVar "A"
                  , TypeVar "B"
                  , MorseEqual
                  , OpenBracket
                  , TypeName "Left"
                  , TypeVar "A"
                  , TypeSeparator
                  , TypeName "Right"
                  , TypeVar "B"
                  , CloseBracket
                  , SemiColon
                  , Type
                  , TypeVar "A"
                  , IdenOp ":|:"
                  , TypeVar "B"
                  , MorseEqual
                  , OpenBracket
                  , TypeName "It"
                  , TypeVar "A"
                  , TypeSeparator
                  , TypeName "This"
                  , TypeVar "B"
                  , CloseBracket
                  , SemiColon
                  , Define
                  , TypeVar "A"
                  , IdenOp ":|:"
                  , TypeVar "B"
                  , Iso
                  , TypeName "Either"
                  , TypeVar "A"
                  , TypeVar "B"
                  , MorseEqual
                  , OpenBracket
                  , Iden "from"
                  , Colon
                  , TypeVar "A"
                  , IdenOp ":|:"
                  , TypeVar "B"
                  , ArrowRight
                  , TypeName "Either"
                  , TypeVar "A"
                  , TypeVar "B"
                  , SemiColon
                  , Iden "from"
                  , Iden "arg"
                  , MorseEqual
                  , OpenBracket
                  , Let
                  , Iden "a+"
                  , MorseEqual
                  , Iden "a+"
                  , IdenOp "+"
                  , Iden "a"
                  , SemiColon
                  , Iden "a+"
                  , IdenOp "+"
                  , IdenOp "+"
                  , Iden "a-"
                  , Iden "a"
                  , CloseBracket
                  , SemiColon
                  , Iden "to"
                  , Colon
                  , TypeName "Either"
                  , TypeVar "A"
                  , TypeVar "B"
                  , ArrowRight
                  , TypeVar "A"
                  , IdenOp ":|:"
                  , TypeVar "B"
                  , SemiColon
                  , Iden "to"
                  , Iden "arg"
                  , MorseEqual
                  , OpenBracket
                  , If
                  , Iden "arg"
                  , Pipe
                  , Iden "some"
                  , BigArrowRight
                  , Iden "test"
                  , SemiColon
                  , CloseBracket
                  , SemiColon
                  ]
