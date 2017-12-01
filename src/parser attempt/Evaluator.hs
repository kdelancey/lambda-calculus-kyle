import Text.Regex.Applicative

    data Name = String
    data Type = Number | Float | Str deriving Show
    data Variable = Name Type deriving Show

    data Function = Print Function
                    | Print Variable
                    | Add Variable Variable
                    | Sub Variable Variable
                    | Div Variable Variable
                    | Mult Variable Variable
                    | Greater Variable Variable
                    | Less Variable Variable
                    | Equal Variable Variable

    data Lambda = [Variable] [Function]

    varible :: RE Char String

    varibles :: RE Char [Variable]
    varibles = sym '&' *> many (psym isPrint) <* string "->"

    lambda = Lambda <$> 
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