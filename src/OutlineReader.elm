module OutlineReader exposing (tree, string, normalString, depth, nodeCount)

import Tree exposing(Tree, singleton)
import Tree.Zipper as Zipper exposing(Zipper    )
import Parser exposing(Parser, succeed, chompWhile, getChompedString, (|.))
import Element exposing(Element(..), level)

type alias Line = String
type alias OutlineText = String
type alias Outline = Tree Element



tree : OutlineText -> Tree Element
tree ot =
    ot
      |> String.lines
      |> List.filter (\l -> String.length l > 0)
      |> List.foldl (\s z -> step s z) z0
      |> Zipper.toTree

t0 = Tree.singleton (Element 0 "*")
z0 = Zipper.fromTree t0

step : String ->  Zipper Element -> Zipper Element
step s z =
    let
        lDiff =  levelDifference s z
        ls = level s
    in
      case lDiff of
        Nothing ->  appendStringAtFocus s z
        Just 0 -> appendStringAtFocus s z
        Just 1 -> addChildAtFocus s z
        _ ->
            Zipper.root z |> appendStringAtFocus s


string : Tree Element -> String
string t =
    let
        c = Tree.children t
        lab_ = Tree.label t
        lev_ = lab_ |> Element.level
        n  = 2 * (lev_ - 1 )
        prefix = String.repeat n " "
        lab =  prefix ++ Element.string lab_
    in
      if c == [] then
       lab
      else
        lab ++ "\n" ++ (List.map string c |> String.join "\n")

normalString : Tree Element -> String
normalString  t =
    String.replace "*" "" (string t)
      |> String.trim



-- ADDING THINGS --


appendStringAtFocus : String -> Zipper Element -> Zipper Element
appendStringAtFocus s z =
    let
        t = Zipper.tree z
        element = Element (level s) (String.trim s)
        newTree = Tree.appendChild (singleton element) t
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

nthParentOfFocus : Int -> Zipper Element -> Zipper Element
nthParentOfFocus k z =
    case manyBackward k z of
        Nothing -> z
        Just zz -> zz

-- MOVING AROUND --

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



-- LEVELS --


levelOfLastChild : Zipper Element -> Maybe Int
levelOfLastChild z =
    Zipper.lastChild z
      |> Maybe.map Zipper.tree
      |> Maybe.map Tree.label
      |> Maybe.map Element.level


levelDifference : String -> Zipper Element -> Maybe Int
levelDifference s z =
     Maybe.map2 (-) (Just <| level s) (levelOfLastChild z)



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




-- STANDARD TREE FUNCTIONS --



depth : Tree Element -> Int
depth t =
    let
       c = Tree.children t
    in
    if c == [] then
      0
    else
     1 + listMax (List.map depth c)

nodeCount : Tree Element -> Int
nodeCount t =
    let
           c = Tree.children t
     in
     if c == [] then
           1
         else
          1 + List.sum (List.map nodeCount c)


listMax : List Int -> Int
listMax ints =
    List.foldl (\i acc -> max i acc) 0 ints


-- DEPRECATED --

{-

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