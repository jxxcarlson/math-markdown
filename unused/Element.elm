module Element exposing (Element(..), level, string)

type Element = Element Int String

level : Element -> Int
level (Element k _ ) = k

string : Element -> String
string (Element _ str) = str