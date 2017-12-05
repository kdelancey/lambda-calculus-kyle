module STParser where

import Text.Regex.Applicative
import Data.Char hiding (Space)
import Data.String
import Data.Maybe

type Name 
    = String
data Type 
    = Number | Float | Str | Bol | X deriving (Enum, Show)
    -- X denotes unknown type. If unknown in variable declaration during evaluation, throw errors
data Variable 
    = Variable Name Type deriving Show

-- Variable Methods --
getName :: Variable -> Name
getName (Variable nm ty) = nm

getType :: Variable -> Type
getType (Variable nm ty) = ty

-- Parser Methods --

whitespace :: RE Char String
whitespace = many(sym ' ')

name :: RE Char Name
name = whitespace *> ((:) <$> psym isAlpha <*> many (psym isAlphaNum))    

type' :: RE Char Type
type' = Number     <$ (whitespace *> string ":" <* whitespace *> string "Int")
        <|> Float  <$ (whitespace *> string ":" <* whitespace *> string "Flt")
        <|> Str    <$ (whitespace *> string ":" <* whitespace *> string "Str")
        <|> Bol    <$ (whitespace *> string ":" <* whitespace *> string "Bol")

variable :: RE Char Expr
variable = Var <$> (Variable <$> name <*> (X <$ whitespace))

variableDec :: RE Char Variable
variableDec = Variable <$> name <*> type'

variableDecs :: RE Char [Variable]
variableDecs = sym '&' *> many variableDec <* (whitespace *> string "->" <* whitespace)

data Lambda 
    = Lambda [Variable] [Expr] deriving Show

data Expr 
        = Print     Expr
        | ILmbd     Lambda
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
expr    =   Print     <$> (string "print" *> variable)
        <|> Add       <$> variable <*> (sym '+' *> variable)
        <|> Sub       <$> variable <*> (sym '-' *> variable)
        <|> Div       <$> variable <*> (sym '/' *> variable)
        <|> Mult      <$> variable <*> (sym '*' *> variable)
        <|> Greater   <$> variable <*> (sym '>' *> variable)
        <|> Less      <$> variable <*> (sym '<' *> variable)
        <|> Equal     <$> variable <*> (sym '=' *> variable)
        <|> variable
        <|> ILmbd     <$> lambda

exprs :: RE Char [Expr]
exprs = many expr

lambda :: RE Char Lambda
lambda = Lambda <$> variableDecs <*> exprs

main = do
    putStrLn "\nTesting different Steps of Parser:"
    putStrLn "\nName:"
    print $ "nameOfAVariable" =~ name
    putStrLn "\nType:"
    print $ ":Int" =~ type'
    putStrLn "\nVariable Declarations:"
    print $ "nameOfAVariable:Int" =~ variableDec
    putStrLn "\nVariable Declarations:"
    print $ "& a   :   Int   b:Int -> " =~ variableDecs
    putStrLn "\nVariable as Expr:"
    print $ "a" =~ variable
    putStrLn "\nExpression:"
    print $ "a * b" =~ expr
    putStrLn "\nSingle Lambda:"
    print $ "& a   :   Int   b:Int -> a * b" =~ lambda
    putStrLn "\nNested Lambdas:"
    print $ "& a:Int   b:Int -> & x:Int y:Int -> x + y a * b" =~ lambda
    putStrLn "\nDone Tests"