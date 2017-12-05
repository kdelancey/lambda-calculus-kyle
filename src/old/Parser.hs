import Text.Regex

-- May help later
-- http://www.serpentine.com/blog/2007/02/27/a-haskell-regular-expression-tutorial/

-- REGULAR EXPRESSIONS --
re_ampsnd = mkRegex "&"
re_lparen = mkRegex "\\("
re_rparen = mkRegex "\\)"
re_quotmk = mkRegex "\""
re_colon  = mkRegex ":"
re_arrow  = mkRegex "->"
re_whtspc = mkRegex "[ \t\n]"
re_less   = mkRegex "<"
re_equal  = mkRegex "=="
re_mult   = mkRegex "*"
re_div    = mkRegex "/"
re_sub    = mkRegex "-"
re_add    = mkRegex "+"
re_ty_int = mkRegex "Int"
re_ty_flt = mkRegex "Flt"
re_ty_str = mkRegex "Str"
re_id     = mkRegex "[a-zA-Z][a-zA-Z]*"
re_vl_int = mkRegex "[-]?[0-9]+"
re_vl_flt = mkRegex "[-]?[0-9]+.[0-9]+"

-- Test to see if there is a valid match at
-- the START of the string.
matchTest :: Regex -> String -> Bool
matchTest regex str = 
    case matchRegexAll regex str of
        Just ("",b,c,[]) -> True
        Nothing          -> False
        otherwise        -> False

parser :: String -> [String]
parser input = parser' input []

parser' :: String -> [String] -> [String]
parser' ""  lst = lst
parser' exp lst =
    if (matchTest re_ampsnd exp) then
        case (matchRegexAll re_ampsnd exp) of
            Just (bfr, m, aft, s) -> parser' aft (lst ++ [m])
            Nothing -> error "BadParse re_ampsnd"
    else if (matchTest re_lparen exp) then
        case (matchRegexAll re_lparen exp) of
            Just (bfr, m, aft, s) -> parser' aft (lst ++ [m])
            Nothing -> error "Bad Parse with: re_lparen"
    else if (matchTest re_rparen exp) then
        case (matchRegexAll re_rparen exp) of
            Just (bfr, m, aft, s) -> parser' aft (lst ++ [m])
            Nothing -> error "Bad Parse with: re_rparen"
    else if (matchTest re_quotmk exp) then
        case (matchRegexAll re_quotmk exp) of
            Just (bfr, m, aft, s) -> parser' aft (lst ++ [m])
            Nothing -> error "Bad Parse with: re_quotmk"               
    else if (matchTest re_colon  exp) then
        case (matchRegexAll re_colon exp) of
            Just (bfr, m, aft, s) -> parser' aft (lst ++ [m])
            Nothing -> error "Bad Parse with: re_colon"
    else if (matchTest re_arrow  exp) then
        case (matchRegexAll re_arrow exp) of
            Just (bfr, m, aft, s) -> parser' aft (lst ++ [m])
            Nothing -> error "Bad Parse with: re_arrow"
    else if (matchTest re_whtspc exp) then
        case (matchRegexAll re_whtspc exp) of
            Just (bfr, m, aft, s) -> parser' aft (lst ++ [m])
            Nothing -> error "Bad Parse with: re_whtspc"
    else if (matchTest re_less   exp) then
        case (matchRegexAll re_less exp) of
            Just (bfr, m, aft, s) -> parser' aft (lst ++ [m])
            Nothing -> error "Bad Parse with: re_less"
    else if (matchTest re_equal  exp) then
        case (matchRegexAll re_equal exp) of
            Just (bfr, m, aft, s) -> parser' aft (lst ++ [m])
            Nothing -> error "Bad Parse with: re_equal"
    else if (matchTest re_mult   exp) then
        case (matchRegexAll re_mult exp) of
            Just (bfr, m, aft, s) -> parser' aft (lst ++ [m])
            Nothing -> error "Bad Parse with: re_mult"
    else if (matchTest re_div    exp) then
        case (matchRegexAll re_div exp) of
            Just (bfr, m, aft, s) -> parser' aft (lst ++ [m])
            Nothing -> error "Bad Parse with: re_div"
    else if (matchTest re_sub    exp) then
        case (matchRegexAll re_sub exp) of
            Just (bfr, m, aft, s) -> parser' aft (lst ++ [m])
            Nothing -> error "Bad Parse with: re_sub"
    else if (matchTest re_add    exp) then
        case (matchRegexAll re_add exp) of
            Just (bfr, m, aft, s) -> parser' aft (lst ++ [m])
            Nothing -> error "Bad Parse with: re_add"
    else if (matchTest re_ty_int exp) then
        case (matchRegexAll re_ty_int exp) of
            Just (bfr, m, aft, s) -> parser' aft (lst ++ [m])
            Nothing -> error "Bad Parse with: re_ty_int"
    else if (matchTest re_ty_flt exp) then
        case (matchRegexAll re_ty_flt exp) of
            Just (bfr, m, aft, s) -> parser' aft (lst ++ [m])
            Nothing -> error "Bad Parse with: re_ty_flt"
    else if (matchTest re_ty_str exp) then
        case (matchRegexAll re_ty_str exp) of
            Just (bfr, m, aft, s) -> parser' aft (lst ++ [m])
            Nothing -> error "Bad Parse with: re_ty_str"
    else if (matchTest re_id     exp) then
        case (matchRegexAll re_id exp) of
            Just (bfr, m, aft, s) -> parser' aft (lst ++ [m])
            Nothing -> error "Bad Parse with: re_id"
    else if (matchTest re_vl_flt exp) then
        case (matchRegexAll re_vl_flt exp) of
            Just (bfr, m, aft, s) -> parser' aft (lst ++ [m])
            Nothing -> error "Bad Parse with: re_vl_flt"
    else if (matchTest re_vl_int exp) then
        case (matchRegexAll re_vl_int exp) of
            Just (bfr, m, aft, s) -> parser' aft (lst ++ [m])
            Nothing -> error "Bad Parse with: re_vl_int"
    else error "Bad Parse, never matched"

melly = parser "& a:Int b:Int -> a + b"