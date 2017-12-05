import Text.Regex.Applicative
import Data.Char hiding (Space)
import Data.String
import Data.Maybe

type Name 
    = String
data Type 
    = Number | Float | Str | X deriving (Enum, Show)
    -- X denotes unknown type. If unknown in variable declaration during evaluation, throw errors
data Variable 
    = Variable Name Type deriving Show

whitespace :: RE Char String
whitespace = many(sym ' ')

name :: RE Char Name
name = whitespace *> ((:) <$> psym isAlpha <*> many (psym isAlphaNum))    

type' :: RE Char Type
type' = Number     <$ (whitespace *> string ":" <* whitespace *> string "Int")
        <|> Float  <$ (whitespace *> string ":" <* whitespace *> string "Flt")
        <|> Str    <$ (whitespace *> string ":" <* whitespace *> string "Str")

variable :: RE Char Variable
variable = Variable <$> name <*> (X <$ whitespace)

variableDec :: RE Char Variable
variableDec = Variable <$> name <*> type'

variableDecs :: RE Char [Variable]
variableDecs = sym '&' *> many variableDec <* (whitespace *> string "->" <* whitespace)

data Lambda 
    = Lambda [Variable] [Expr] deriving Show

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
        deriving Show

expr :: RE Char Expr
expr    =   Print     <$> (string "print" *> expr)
        <|> Var       <$> variable
        <|> Lamb      <$> lambda
        <|> Add       <$> expr <* sym '+' *> expr
        <|> Sub       <$> expr <* sym '-' *> expr
        <|> Div       <$> expr <* sym '/' *> expr
        <|> Mult      <$> expr <* sym '*' *> expr
        <|> Greater   <$> expr <* sym '>' *> expr
        <|> Less      <$> expr <* sym '<' *> expr
        <|> Equal     <$> expr <* sym '=' *> expr

exprs :: RE Char [Expr]
exprs = many expr

lambda :: RE Char Lambda
lambda = Lambda <$> variableDecs <*> exprs

main = do 
    print "Testing different RegExp:"
    print "Name:"
    print $ "nameOfAVariable" =~ name
    print "Type:"
    print $ ":Int" =~ type'
    print "Variable Declarations:"
    print $ "nameOfAVariable:Int" =~ variableDec
    print "Variable Declarations:"
    print $ "& a   :   Int   b:Int -> " =~ variableDecs
    print "Expression:"
    print $ "a * b" =~ expr
    print "Single Lambda:"
    print $ "& a   :   Int   b:Int -> a " =~ lambda
    print "Done Tests"

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