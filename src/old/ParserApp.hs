-- Building from:
-- https://github.com/feuerbach/regex-applicative/wiki/Examples

import Text.Regex.Applicative
import Data.Char hiding (Space)
import Data.String
import Data.Maybe

data LType
    = Number
    | Float
    | Str
    deriving Show

data Token
    = Tok_Number Int
    | Tok_Flt String --Double
    | Tok_Str String
    | Tok_IntOpr Char
    | Tok_BlnOpr String
    | Tok_Id String
    | Tok_Type String
    | Tok_Arrow
    | Tok_Lambda
    | Tok_LParen
    | Tok_RParen
    deriving Show

-- TODO: Make a mathcing LType similar to matiching String in 'string'
--       aka, map Strings (:Int, :Float, :String) to LTypes

-- Match Chars with 'isDigit', convert to Int
num :: RE Char Int
num = read <$> many (psym isDigit)

-- Regular Expression matching any substring
-- that fits inside a set of quotation marks
str :: RE Char String
str = sym '"' *> many (psym isPrint) <* sym '"'

-- Regular Expression matching any substring
-- that has digits before and after a period,
-- representing a float
-- flt :: RE Char [Char]
-- flt = many (psym isDigit) <* sym '.' *> many (psym isDigit)
-- realToFrac To

-- Map 'string' over all boolean operator symbols,
-- then make a Left-Associated RE match with symbols
blnOpr :: RE Char [Char]
blnOpr = foldr1 (<|>) $ map string ["==", "<", ">"]

-- Map 'sym' over all int operator symbols,
-- then make a Left-Associated RE match with symbols
intOpr :: RE Char Char
intOpr = foldr1 (<|>) $ map sym ['+', '-', '/', '*']

-- Map 'string' over all boolean operator symbols,
-- then make a Left-Associated RE match with symbols
typeIdentifier :: RE Char [Char]
typeIdentifier = foldr1 (<|>) $ map string [":Int", ":Flt", ":Str", ":Char"]

-- Regular Expression matching a substring
-- starting with a letter, and continuing with
-- any combination of letters and numbers until
-- whitespace or another Lex symbol
identifier :: RE Char String
identifier = (:) <$> psym isAlpha <*> many (psym isAlphaNum)

-- Regular Expression for Whitespace.
space :: RE Char String
space = many $ psym isSpace

-- Left-Associated Lexer
token :: RE Char Token
token = 
     (Tok_Number <$> num)
     <|> (Tok_Str <$> str)
     <|> (Tok_IntOpr <$> intOpr)
     <|> (Tok_BlnOpr <$> blnOpr)
     <|> (Tok_Id <$> identifier)
     <|> (Tok_Type <$> typeIdentifier)
     <|> (Tok_Arrow <$ string "->")
     <|> (Tok_Lambda <$ sym '&')
     <|> (Tok_LParen <$ sym '(')
     <|> (Tok_RParen <$ sym ')')

         --  (Tok_Flt <$> flt)
    --  <|> 

tokens :: RE Char [Token]
tokens = catMaybes <$> many ((Just <$> token) <|> (Nothing <$ space))

main = print $ "& a:Int b:Int -> \"ayylma\" + b" =~ tokens