module Element exposing (..)

type Element = Element Int String

level : Element -> Int
level (Element k _ ) = k

string : Element -> String
string (Element _ str) = str