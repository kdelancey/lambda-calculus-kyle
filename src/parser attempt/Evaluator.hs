import Text.Regex.Applicative
import Data.Char hiding (Space)
import Data.String
import Data.Maybe

type Name 
    = String
data Type 
    = Number | Float | Str deriving (Enum, Show)
data Variable 
    = Variable Name Type deriving Show

name :: RE Char Name
name = (:) <$> psym isAlpha <*> many $ psym isAlphaNum    

type' :: RE Char Type
type' = Number <$ string ":Int" 
        <|> Float  <$ string ":Flt"
        <|> Str    <$ string ":Str"

variable :: RE Char Variable
variable = Variable <$> name <*> type'

variables :: RE Char [Variable]
variables = sym '&' *> many $ variable <* string "->"

data Lambda 
    = Lambda [Variable] [Expr]

data Expr 
        = Print     Expr
        | Lamb      Lambda
        | Var       Variable
        | Add       Expr Expr
        | Sub       Expr Expr
        | Div       Expr Expr
        | Mult      Expr Expr
        | Greater   Expr Expr
        | Less      Expr Expr
        | Equal     Expr Expr

expr :: RE Char Expr
expr    =   Print     <$> expr
        <|> Lamb      <$> lambda
        <|> Var       <$> variable
        <|> Add       <$> expr <*> expr
        <|> Sub       <$> expr <*> expr
        <|> Div       <$> expr <*> expr
        <|> Mult      <$> expr <*> expr
        <|> Greater   <$> expr <*> expr
        <|> Less      <$> expr <*> expr
        <|> Equal     <$> expr <*> expr

exprs :: RE Char [Expr]
exprs = many $ expr

lambda :: RE Char Lambda
lambda = Lambda <$> variables <*> exprs

-- protocol :: RE Char Protocol
-- protocol = HTTP <$ string "http" <|> FTP <$ string "ftp"

-- type Host = String
-- type Location = String
-- data URL = URL Protocol Host Location deriving Show

-- host :: RE Char Host
-- host = many $ psym $ (/= '/')

-- url :: RE Char URL
-- url = URL <$> protocol <* string "://" <*> host <* sym '/' <*> many anySym

-- main = print $ "http://stackoverflow.com/questions" =~ url