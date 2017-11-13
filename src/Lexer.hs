module LambdaLexer where
    import System.IO
    import Control.Monad
    import Text.ParserCombinators.Parsec
    import Text.ParserCombinators.Parsec.Expr
    import Text.ParserCombinators.Parsec.Language
    import qualified Text.ParserCombinators.Parsec.Token as Token

    -- data BExpr = BoolConst Bool
    --             | Not BExpr
    --             | BBinary BBinOp BExpr BExpr
    --             | RBinary RBinOp AExpr AExpr
    --             deriving (Show)

    -- data BBinOp = And | Or deriving (Show)

    -- data RBinOp = Greater | Less deriving (Show)

    languageDef =
          emptyDef
          { Token.commentStart   = "~*"
          , Token.commentEnd     = "*~"
          , Token.commentLine    = "~~"
          , Token.nestedComments = True
          , Token.identStart     = letter
          , Token.identLetter    = alphaNum <|> oneOf "_"
          , Token.opStart        = opLetter emptyDef
          , Token.opLetter       = oneOf "!%&*+.<=>^|-"
          , Token.reservedOpNames= []
          , Token.reservedNames  = ["&", ":"]
          , Token.caseSensitive  = True
          }

    lexer = Token.makeTokenParser languageDef

          