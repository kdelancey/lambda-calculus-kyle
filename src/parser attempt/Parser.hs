
import Text.Regex

-- REGULAR EXPRESSIONS --
re_ampsnd = "a"

parser :: String -> [String]
parser input = words input

melly = parser "& 3 () ABC"