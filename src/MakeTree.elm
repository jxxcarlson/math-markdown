module MakeTree exposing (fromList, index, depth, nodeCount)

import Tree exposing(Tree, singleton)
import Tree.Zipper as Zipper exposing(Zipper    )
import Parser exposing(Parser, succeed, chompWhile, getChompedString, (|.))
import Element exposing(Element(..), level)


type alias Line = String
type alias OutlineText = String
type alias Outline = Tree Element

{-|



-}

fromList : Tree a -> (a -> Int) -> (List a) -> Tree a
fromList initialTree level lst =
    lst
      |> List.foldl (\s z -> step level s z) (Zipper.fromTree initialTree)
      |> Zipper.toTree

step : (a -> Int) -> a ->  Zipper a -> Zipper a
step level s z =
    let
        lDiff =  levelDifference level s z
        ls = level s
    in
      case lDiff of
        Nothing ->  appendElementFocus level s z
        Just 0 -> appendElementFocus level s z
        Just 1 -> addChildAtFocus level s z
        _ ->
            Zipper.root z |> appendElementFocus level s


-- ADDING THINGS --


appendElementFocus : (a -> Int) -> a -> Zipper a -> Zipper a
appendElementFocus level s z =
    let
        t = Zipper.tree z
        newTree = Tree.appendChild (singleton s) t
    in
      Zipper.replaceTree newTree z


addChildAtFocus : (a -> Int) -> a -> Zipper a -> Zipper a
addChildAtFocus level s z =
    case Zipper.lastChild z of
        Nothing -> z
        Just zz -> appendElementFocus level s zz

addChildAtNthParentOfFocus : (a -> Int) -> Int -> a -> Zipper a -> Zipper a
addChildAtNthParentOfFocus level k s z =
    case manyBackward k z of
        Nothing -> z
        Just zz -> appendElementFocus level s zz

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


levelOfLastChild : (a -> Int) -> Zipper a -> Maybe Int
levelOfLastChild level z =
    Zipper.lastChild z
      |> Maybe.map Zipper.tree
      |> Maybe.map Tree.label
      |> Maybe.map level


levelDifference : (a -> Int) -> a -> Zipper a -> Maybe Int
levelDifference level s z =
     Maybe.map2 (-) (Just <| level s) (levelOfLastChild level z)




-- STANDARD TREE FUNCTIONS --



depth : Tree a -> Int
depth t =
    let
       c = Tree.children t
    in
    if c == [] then
      0
    else
     1 + listMax (List.map depth c)


index : Tree String -> Tree (String, Int)
index t =
    index_ (t, 0)

index_ : (Tree String,  Int) -> Tree (String, Int)
index_ (t, k) =
    let
        c = Tree.children t
     in
        Tree.tree (Tree.label t, k) (List.map (\t_ -> index_ (t_, (k + 1))) c)

nodeCount : Tree a -> Int
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

