module MakeTreeExample exposing (parse, parseZ, string, normalString, stringLevel, test, o1, o2, o3, o3a, o3b
   , o4, o4a, o4x, o4y, o4z, o4w)

import HTree
import Tree exposing(Tree)
import Parser exposing(Parser, succeed, chompWhile, getChompedString, (|.))

{-

    > t = parse o3
    Tree "*" [Tree "A" [Tree "p" [Tree "1" [],Tree "2" [],Tree "3" []]],Tree "q" [],Tree "B" [Tree "r" [],Tree "s" []],Tree "C" []]

    > t = parse o3 |> HTree.tag
    Tree ("*",0) [Tree ("A",1) [Tree ("p",2) [Tree ("1",3) [],Tree ("2",3) [],Tree ("3",3) []]],Tree ("q",1) [],Tree ("B",1) [Tree ("r",2) [],Tree ("s",2) []],Tree ("C",1) []]

-}

parse : String -> Tree String
parse str =
    String.split "\n" str
      |> List.filter (\s -> s /= "")
      |> HTree.fromList initialTreeS stringLevel
      |> Tree.map (\l -> String.trim l)


-- parseZ : String -> Zipper String
parseZ str =
    String.split "\n" str
      |> List.filter (\s -> s /= "")
      |> HTree.fromListZ initialTreeS stringLevel

normalString : Tree String -> String
normalString t =
    t |> HTree.tag |> normalString_

normalString_ : Tree (String, Int) -> String
normalString_ t =
    String.dropLeft 2 (string t)

string : Tree (String, Int) -> String
string t =
    let
        c = Tree.children t
        (label_, level_) = Tree.label t
        n  = 2 * (level_ - 1 )
        prefix = String.repeat n " "
        lab =  prefix ++ label_
    in
      if c == [] then
       lab
      else
        lab ++ "\n" ++ (List.map string c |> String.join "\n")

test : String -> Bool
test str =
    let
       a = String.trim str
       b = parse str |> normalString |> String.trim
     in
       a == b

-- HELPERS --

initialTreeS = Tree.singleton "*"

stringLevel : String -> Int
stringLevel str =
    Parser.run numberOfLeadingBlanks str
      |> Result.toMaybe
      |> Maybe.map (\l -> 1 + l//2)
      |> Maybe.withDefault 0


numberOfLeadingBlanks : Parser Int
numberOfLeadingBlanks =
    (succeed ()
        |. chompWhile (\c -> c == ' ')
    )
        |> getChompedString
        |> Parser.map String.length



-- EXAMNPELES --


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

o3a =
    """A
  p
    1
    2
    3
"""

o3b =
    """A
  p
    1
    2
    3
  q
"""


o4 =
    """A
  p
    1
    2
    3
      alpha
      beta
  q
B
  r
  s
C
"""

o4x =
    """A
  p
    1
    2
    3
      alpha
      beta
"""

o4y =
    """A
  p
    1
    2
    3
      alpha
      beta
    y
"""

o4z =
    """A
  p
    1
    2
    3
      alpha
      beta
  z
"""

o4w =
    """A
  p
    1
    2
    3
      alpha
      beta
w
"""

o4a =
    """A
  p
    1
    2
    3
      alpha
      beta
    X
  q
B
  r
  s
C
"""