module OutlineReader exposing (..)

import Tree exposing(Tree, tree, singleton)
import Tree.Zipper as Zipper exposing(Zipper    )
import Parser exposing(Parser, succeed, chompWhile, getChompedString, (|.))
import Maybe.Extra
import Element exposing(..)

type alias Line = String
type alias OutlineText = String
type alias Outline = Tree Element



{-
ALGORITHM.

Set current node to Node * []
Set current level to 0
Read line and find level
If line level = current level, add the line as Leaf Line to the current node's list
If line level < current level,




-}

-- EXAMPLES --

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


o2b = """
A
  c
  d
E
"""

o2bx = """
A
  c
  d
"""

o2c =
    """
A
B
  p
  q
C
"""

o2cx =
    """
A
B
  p
  q
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


-- FUNCTIONS --


t0 = Tree.singleton (Element 0 "*")
z0 = Zipper.fromTree t0


--depth : Tree Element -> Int
--depth t =
--    case t of
--        ((Tree Element) _ []) -> 0
--        ((Tree a) _ children) -> 1 + listMax (List.map depth children)
--
--zDepth : Zipper a -> Int
--zDepth z =
--    Zipper.toTree z |> depth


listMax : List Int -> Int
listMax ints =
    List.foldl (\i acc -> max i acc) 0 ints

{-|

    > t0 = tree (Element 0 "*") []
    Tree (Element 0 "*") [] : Tree Element

    > z0 = Zipper.fromTree t0
    Zipper { after = [], before = [], crumbs = [], focus = Tree (Element 0 "*") [] }
      : Zipper Element

    > appendStringAtFocus "A" z0
    Zipper { after = [], before = [], crumbs = [], focus = Tree (Element 0 "*") [Tree (Element 1 "A") []] }
      : Zipper Element

    > appendStringAtFocus "A" z0 |> appendStringAtFocus "B"
    Zipper { after = [], before = [], crumbs = [], focus = Tree (Element 0 "*") [Tree (Element 1 "A") [],Tree (Element 1 "B") []] }
        : Zipper Element

 -}
appendStringAtFocus : String -> Zipper Element -> Zipper Element
appendStringAtFocus s z =
    let
        t = Zipper.tree z
        element = Element (level s) (String.trim s)
        newTree = Tree.appendChild (singleton element) t
    in
      Zipper.replaceTree newTree z

prependStringAtFocus : String -> Zipper Element -> Zipper Element
prependStringAtFocus s z =
    let
        t = Zipper.tree z
        element = Element (level s) (String.trim s)
        newTree = Tree.prependChild (singleton element) t
    in
      Zipper.replaceTree newTree z

addChildAtFocus : String -> Zipper Element -> Zipper Element
addChildAtFocus s z =
    case Zipper.lastChild z of
        Nothing -> z
        Just zz -> appendStringAtFocus s zz

addChildAtNthParentOfFocus : Int -> String -> Zipper Element -> Zipper Element
addChildAtNthParentOfFocus k s z =
    case manyBackward k z of
        Nothing -> z
        Just zz -> appendStringAtFocus s zz

appendStringAtNthParentOfFocus : Int -> String -> Zipper Element -> Zipper Element
appendStringAtNthParentOfFocus k s z =
    case manyBackward k z of
        Nothing -> z
        Just zz -> appendStringAtFocus s zz

manyBackward : Int -> Zipper a -> Maybe (Zipper a)
manyBackward k z =
   if k < 0 then
     Nothing
   else if k == 0 then
     Just z
   else
     let
         zz = Zipper.backward z
      in
        iterate k (\zi -> Maybe.andThen Zipper.backward zi) zz



iterate : Int -> (a -> a) -> a -> a
iterate k f x =
    List.foldl (\i acc -> f acc) x (List.range 1 k)


levelOfLastChild : Zipper Element -> Maybe Int
levelOfLastChild z =
    Zipper.lastChild z
      |> Maybe.map Zipper.tree
      |> Maybe.map Tree.label
      |> Maybe.map Element.level


levelDifference : String -> Zipper Element -> Maybe Int
levelDifference s z =
     Maybe.map2 (-) (Just <| level s) (levelOfLastChild z)

tree : OutlineText -> Tree Element
tree ot =
    ot
      |> String.lines
      |> List.filter (\l -> String.length l > 0)
      |> List.foldl (\s z -> step s z) z0
      |> Zipper.toTree

zipper : OutlineText -> Zipper Element
zipper ot =
    ot
      |> String.lines
      |> List.filter (\l -> String.length l > 0)
      |> List.foldl (\s z -> step s z) z0

step : String ->  Zipper Element -> Zipper Element
step s z =
    let
        n =  levelDifference s z
    in
      case n of
        Nothing ->  appendStringAtFocus s z
        Just 0 -> appendStringAtFocus s z
        Just 1 -> addChildAtFocus s z
        _ ->
           case Maybe.map negate n of
               Just k ->addChildAtNthParentOfFocus k s z
               _ -> z



numberOfLeadingBlanks : Parser Int
numberOfLeadingBlanks =
    (succeed ()
        |. chompWhile (\c -> c == ' ')
    )
        |> getChompedString
        |> Parser.map String.length

level : Line -> Int
level ln =
    Parser.run numberOfLeadingBlanks ln
      |> Result.toMaybe
      |> Maybe.map (\l -> 1 + l//2)
      |> Maybe.withDefault 0


levels : OutlineText -> List Int
levels outline =
    outline
      |> String.lines
      |> List.filter (\l -> String.length l > 0)
      |> List.map level


{-

TEST:

    > t0 = tree (Element 0 "*") []
    Tree (Element 0 "*") [] : Tree Element

    > z0 = Zipper.fromTree t0
    Zipper { after = [], before = [], crumbs = [], focus = Tree (Element 0 "*") [] }
          : Zipper Element

    > z1 = step "B" <| step "A" z0
    Zipper { after = [], before = [], crumbs = [], focus = Tree (Element 0 "*") [Tree (Element 1 "A") [],Tree (Element 1 "B") []] }
        : Zipper Element.Element
    > t1 = Zipper.toTree z1
    Tree (Element 0 "*") [Tree (Element 1 "A") [],Tree (Element 1 "B") []]

    > z1 = step "B" <| step "A" z0
    Zipper { after = [], before = [], crumbs = [], focus = Tree (Element 0 "*") [Tree (Element 1 "A") [],Tree (Element 1 "B") []] }
        : Zipper Element.Element
    > t1 = Zipper.toTree z1
    Tree (Element 0 "*") [Tree (Element 1 "A") [],Tree (Element 1 "B") []]
        : Tree.Tree Element.Element

    > z2 =  step "  c"  z1
    Zipper { after = [], before = [Tree (Element 1 "A") []], crumbs = [{ after = [], before = [], label = Element 0 "*" }], focus = Tree (Element 1 "B") [Tree (Element 2 "c") []] }
        : Zipper Element.Element
    > t2 = Zipper.toTree z2
    Tree (Element 0 "*") [Tree (Element 1 "A") [],Tree (Element 1 "B") [Tree (Element 2 "c") []]]

    > z3 =  step "  d"  z2
    Zipper { after = [], before = [Tree (Element 1 "A") []], crumbs = [{ after = [], before = [], label = Element 0 "*" }], focus = Tree (Element 1 "B") [Tree (Element 2 "c") [],Tree (Element 2 "d") []] }
        : Zipper Element.Element
    > t3 = Zipper.toTree z3
    Tree (Element 0 "*") [Tree (Element 1 "A") [],Tree (Element 1 "B") [Tree (Element 2 "c") [],Tree (Element 2 "d") []]]

    > z4 = step "E" z3
    Zipper { after = [], before = [], crumbs = [], focus = Tree (Element 0 "*") [Tree (Element 1 "A") [],Tree (Element 1 "B") [Tree (Element 2 "c") [],Tree (Element 2 "d") []],Tree (Element 1 "E") []] }
        : Zipper Element.Element

    > t4 = Zipper.tree z4
    Tree (Element 0 "*") [Tree (Element 1 "A") [],Tree (Element 1 "B") [Tree (Element 2 "c") [],Tree (Element 2 "d") []],Tree (Element 1 "E") []]



-}