module Forest exposing (..)

{-  The Forest module is for self-teaching purposes: to
understand some basic issues about data structures before
embarking on the construction of a new parser.

Here a Forest is a tree in which (a) nodes have content,
(b) nodes can be terminal or can have arbitrarily many children.
-}

type Forest a = Leaf a | Node a (List (Forest a))

f1 = Leaf "red"
f2 = Leaf "green"
f3 = Leaf "blue"

f4 = Node "colors" [f1, f2, f3]
f5 = Node "sounds" [Leaf "soft", Leaf "loud"]

f6 = Node "qualities" [f4, f5]

nodeCount : Forest a -> Int
nodeCount f =
    case f of
      (Leaf _) -> 1
      (Node _ subforests) -> 1 + List.sum (List.map nodeCount subforests)


leafCount : Forest a -> Int
leafCount f =
    case f of
      (Leaf _) -> 1
      (Node _ subforests) -> List.sum (List.map leafCount subforests)

depth : Forest a -> Int
depth f =
    case f of
        (Leaf _) -> 0
        (Node _ subforests) -> 1 + listMax (List.map depth subforests)

listMax : List Int -> Int
listMax ints =
    List.foldl (\i acc -> max i acc) 0 ints

addSibling : a -> Forest a -> Forest a
addSibling x f =
    f