module OutlineReader exposing (..)

import Forest exposing(..)
import Parser exposing(Parser, succeed, chompWhile, getChompedString, (|.))
import Maybe.Extra

o1 =
    """A
B
C
"""

o2 =
    """A
  p
  q
B
  r
  s
C
"""

o3 =
    """A
  p
    1
    2
    3
  q
B
  r
  s
C
"""

type alias Line = String
type alias Outline = String

numberOfLeadingBlanks : Parser Int
numberOfLeadingBlanks =
    (succeed ()
        |. chompWhile (\c -> c == ' ')
    )
        |> getChompedString
        |> Parser.map String.length

level : Line -> Maybe Int
level ln =
    Parser.run numberOfLeadingBlanks ln
      |> Result.toMaybe
      |> Maybe.map (\l -> l//2)

levels : Outline -> List Int
levels outline =
    outline
      |> String.lines
      |> List.filter (\l -> String.length l > 0)
      |> List.map level
      |> Maybe.Extra.values

