module HTree exposing (fromList, fromListZ, tag, toList, depth
   , nodeCount, nthParentOfFocus, iterate, manyParent, levelDifference
   , step)

{-|

The purpose of this module is to transform hierarchical lists
into trees, where trees are as in `zwilias/elm-rosetree`.  A hierarchical
list is a thing like

```
   lst : List Item
```

endowed with a function


```
   level : Item -> Int
```

and an initial tree, e.g.,

```
   singleton "*"
```

in the case of lists of strings.

The `level` function, which associates non-negative
numbers to items, defines the hierarchy. As an example,
consider the list of lines
of an outline:

```
Plants
   Without Flowers
   With Flowers
Animals
   Invertebrates
      Works
      Insects
   Vertebrates
      Cold-blooded
      Warm-blooded
```

Each line in the list has a level: the number of leading spaces
divided by 3. For another example, consider the list

```
   [("Plants", 1), ("Without Flowers", 2), ... ]
```

The function HTree.fromList converts a hierarchical list into
a tree of items. See HTreeExamplem.

@docs fromList, tag, depth, nodeCount

-}


import Tree exposing(Tree, singleton)
import Tree.Zipper as Zipper exposing(Zipper)



{-|  fromList takes three arguments: an initial tree of items
of type `a`, a function `level` which gives the level of an item,
and a list of items.  It returns the associated tree.


-}
fromList : a -> (a -> Int) -> (List a) -> Tree a
fromList rooLabel  level lst =
    lst
      |> List.foldl (\s z -> step level s z) (Zipper.fromTree (Tree.singleton rooLabel))
      |> Zipper.toTree

fromListZ : a -> (a -> Int) -> (List a) -> Zipper a
fromListZ rooLabel level lst =
    lst
      |> List.foldl (\s z -> step level s z) (Zipper.fromTree (Tree.singleton rooLabel))

step : (a -> Int) -> a ->  Zipper a -> Zipper a
step level s z =
    let
        ld = levelDifference level s z
    in
      case ld of
        Nothing ->  appendAtFocus level s z
        Just 0 -> appendAtFocus level s z
        Just 1 -> addChildAtFocus level s z
        _ ->
           let
              levelsBack = Debug.log "LB" <| (negate (ld |> Maybe.withDefault 0))

           in
           addAtNthParent level levelsBack s z


{-| The `tag` function transforms a tree of items into
a tree of tuples of the form `(a, k)`, where `k` is the
depth of `a` in the tree.
-}
tag : Tree a -> Tree (a, Int)
tag t =
    tag_ (t, 0)

tag_ : (Tree a,  Int) -> Tree (a, Int)
tag_ (t, k) =
    let
        c = Tree.children t
     in
        Tree.tree (Tree.label t, k) (List.map (\t_ -> tag_ (t_, (k + 1))) c)

toList : Tree (a, Int) -> List (a, Int)
toList t =
     Tree.flatten t

-- ADDING THINGS --


appendAtFocus : (a -> Int) -> a -> Zipper a -> Zipper a
appendAtFocus level s z =
    let
        t = Zipper.tree z
        newTree = Tree.appendChild (singleton s) t
    in
      Zipper.replaceTree newTree z


addChildAtFocus : (a -> Int) -> a -> Zipper a -> Zipper a
addChildAtFocus level s z =
    case Zipper.lastChild z of
        Nothing -> z
        Just zz -> appendAtFocus level s zz

addChildAtParentOfFocus : (a -> Int) -> a -> Zipper a -> Zipper a
addChildAtParentOfFocus level s z =
    case Zipper.parent z of
        Nothing -> z
        Just zz -> appendAtFocus level s zz

addAtNthParent : (a -> Int) -> Int -> a -> Zipper a -> Zipper a
addAtNthParent level k s z =
    case manyParent k z of
        Nothing -> z
        Just zz -> appendAtFocus level s zz

nthParentOfFocus : Int -> Zipper a -> Zipper a
nthParentOfFocus k z =
    case manyParent k z of
        Nothing -> z
        Just zz -> zz

-- MOVING AROUND --

manyParent : Int -> Zipper a -> Maybe (Zipper a)
manyParent k z =
     let
         zz = Zipper.parent z
      in
        iterate (k-1) (\zi -> Maybe.andThen Zipper.parent zi) zz


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


{-| depth t is the depth of the tree.  -}
depth : Tree a -> Int
depth t =
    let
       c = Tree.children t
    in
    if c == [] then
      0
    else
     1 + listMax (List.map depth c)


{-| nodecount t is the number of notes in the tree t -}
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

