-- module LambdaLexer where
--     import Text.Parsec
--     import Text.Parsec.Combinator (between, sepBy1, chainr1)
--     import Data.List (elemIndex)


    -- import Text.ParserCombinators.ReadP

    -- & ( x int y int ) x * ( & ( z int ) z * 3 ) 4

    -- lambda = many ( between (munch (string "& (")) (munch (string ")")) (many ( char ) ) )

    -- beginning = string 

    lambda = many ( between (string "& (") (string ")") (many (noneOf "\n\r")) )

    -- parseLambda input = parse lambda "(unknown)" input

    -- programFile = endBy line eol
    -- line = sepBy lambdaExpr (char '&')
    -- lambdaExpr =  many (noneOf "\n\r")

    -- eol =   try (string "\n\r")
    --     <|> try (string "\r\n")
    --     <|> string "\n"
    --     <|> string "\r"
    --     <?> "end of line"

    -- parseLambda :: String -> [[String]]
    -- parseLambda input = output
    --     where   output = case flex of
    --                         Left x -> [["Err"]]
    --                         Right y -> y
    --             flex = parse programFile "(unknown)" input
    -- 
