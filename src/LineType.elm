module LineType exposing (BlockType(..), BalancedType(..), MarkdownType(..), Level, parse, get, dropLeadingBlanks
   , isBalanced)

import Parser.Advanced exposing (..)

type alias Parser a =
    Parser.Advanced.Parser Context Problem a


type Context
    = Definition String
    | List
    | Record


type Problem
    = Expecting String



type BlockType
    = BalancedBlock BalancedType
    | MarkdownBlock MarkdownType

type BalancedType =
   DisplayCode | Verbatim | DisplayMath


isBalanced : BlockType -> Bool
isBalanced bt =
    case bt of
        (BalancedBlock _) -> True
        (MarkdownBlock _) -> False

type MarkdownType =
     UListItem
  | OListItem
  | Bold
  | Italic
  | Plain
  | Image
  | Blank



type alias Level = Int
type alias Line = String

get : String -> (Level, Maybe BlockType )
get str =
    if str == "\n" then
      (0, Just (MarkdownBlock Blank))
    else
    case run parse (dropLeadingBlanks str) of
        Ok result -> (level str, Just result)
        Err _ -> (0, Just (MarkdownBlock Plain))


numberOfLeadingBlanks : Parser Int
numberOfLeadingBlanks =
    (succeed ()
        |. chompWhile (\c -> c == ' ')
    )
        |> getChompedString
        |> map String.length

getNumberOfLeadingBlanks : String -> Int
getNumberOfLeadingBlanks str =
    run numberOfLeadingBlanks str
      |> Result.toMaybe
      |> Maybe.withDefault 0

dropLeadingBlanks : String -> String
dropLeadingBlanks str =
    String.dropLeft (getNumberOfLeadingBlanks str) str

level : Line -> Int
level ln =
    run numberOfLeadingBlanks ln
      |> Result.toMaybe
      |> Maybe.map (\l -> 1 + l//2)
      |> Maybe.withDefault 0

parse : Parser BlockType
parse =
    oneOf [ imageBlock
           , mathBlock
           , unorderedListItemBlock
           , backtrackable verbatimBlock
           , codeBlock

           ]

codeBlock : Parser BlockType
codeBlock =
    (succeed (BalancedBlock DisplayCode)
        |. symbol (Token "```" (Expecting "Expecting four ticks to begin verbatim block"))
    )

verbatimBlock : Parser BlockType
verbatimBlock =
    (succeed (BalancedBlock Verbatim)
        |. symbol (Token "````" (Expecting "Expecting four ticks to begin verbatim block"))
    )

mathBlock : Parser BlockType
mathBlock =
    (succeed (BalancedBlock DisplayMath)
        |. symbol (Token "$$" (Expecting "Expecting four ticks to begin verbatim block"))

    )

imageBlock : Parser BlockType
imageBlock =
    (succeed (MarkdownBlock Image)
        |. symbol (Token "![" (Expecting "Expecting '![' to begin image block"))
        )

unorderedListItemBlock : Parser BlockType
unorderedListItemBlock =
    succeed (MarkdownBlock UListItem)
        |. symbol (Token "- " (Expecting "Expecting '-' to begin list item"))



parseWhile : (Char -> Bool) -> Parser String
parseWhile accepting =
    chompWhile accepting |> getChompedString

