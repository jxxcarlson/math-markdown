module HTree.String exposing (parse, parseToZipper, toString, toNormalString, toNormalString2, level, test)



import HTree
import Tree exposing(Tree)
import Tree.Zipper as Zipper exposing(Zipper)
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
      |> HTree.fromList "*" level
      |> Tree.map (\l -> String.trim l)


parseToZipper : String -> Zipper String
parseToZipper str =
    String.split "\n" str
      |> List.filter (\s -> s /= "")
      |> HTree.fromListZ "*" level

toNormalString : Tree String -> String
toNormalString t =
    t |> HTree.tag |> toNormalString2

toNormalString2 : Tree (String, Int) -> String
toNormalString2 t =
    String.dropLeft 2 (toString t)

toString : Tree (String, Int) -> String
toString t =
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
        lab ++ "\n" ++ (List.map toString c |> String.join "\n")

test : String -> Bool
test str =
    let
       a = String.trim str
       b = parse str |> toNormalString |> String.trim
     in
       a == b

-- HELPERS --

level : String -> Int
level str =
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


