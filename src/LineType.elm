module LineType exposing (BlockType(..), BalancedType(..), MarkdownType(..), parse)

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

type MarkdownType =
     UListItem
  | OListItem
  | Bold
  | Italic
  | Plain
  | Image

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

