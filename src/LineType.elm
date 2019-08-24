module LineType exposing
    ( BalancedType(..)
    , BlockType(..)
    , Level
    , MarkdownType(..)
    , dropLeadingBlanks
    , get
    , isBalanced
    , isMarkDown
    , level
    , parse
    , stringOfBlockType
    )

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


type BalancedType
    = DisplayCode
    | Verbatim
    | DisplayMath



type MarkdownType
    = UListItem
    | OListItem
    | Heading Int
    | HorizontalRule
    | Quotation
    | Poetry
    | Plain
    | Image
    | Blank


stringOfBlockType : BlockType -> String
stringOfBlockType bt =
    case bt of
        BalancedBlock bt_ -> stringOfBalancedType bt_
        MarkdownBlock mt -> stringOfMarkDownType mt


stringOfBalancedType : BalancedType -> String
stringOfBalancedType bt =
    case bt of
      DisplayCode -> "DisplayCode"
      Verbatim -> "Verbatim"
      DisplayMath -> "DisplayMath"

stringOfMarkDownType : MarkdownType -> String
stringOfMarkDownType mt =
    case mt of
        UListItem -> "UListItem"
        OListItem -> "OListItem"
        Heading _ -> "Heading"
        HorizontalRule -> "HorizontalRule"
        Poetry -> "Poetry"
        Quotation -> "Quotation"
        Plain -> "Plain"
        Image -> "Image"
        Blank -> "Blank"

isBalanced : BlockType -> Bool
isBalanced bt =
    case bt of
        BalancedBlock _ ->
            True

        MarkdownBlock _ ->
            False


isMarkDown : BlockType -> Bool
isMarkDown bt =
    case bt of
        BalancedBlock _ ->
            False

        MarkdownBlock _ ->
            True


type alias Level =
    Int


type alias Line =
    String


get : String -> ( Level, Maybe BlockType )
get str =
    if str == "\n" then
        ( 0, Just (MarkdownBlock Plain) )

    else
        case run parse (dropLeadingBlanks str) of
            Ok result ->
                ( level str, Just result )

            Err _ ->
                ( 0, Just (MarkdownBlock Plain) )


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
        |> Maybe.map (\l -> 1 + l // 2)
        |> Maybe.withDefault 0


parse : Parser BlockType
parse =
    oneOf
        [ imageBlock
        , mathBlock
        , unorderedListItemBlock
        , orderedListItemBlock
        , quotationBlock
        , poetryBlock
        , backtrackable verbatimBlock
        , codeBlock
        , headingBlock
        , horizontalRuleBlock
        ]


-- PARSERS --

poetryBlock : Parser BlockType
poetryBlock =
    (succeed ()
        |. symbol (Token ">> " (Expecting "expecting '>> ' to begin poetry block"))
    )
        |> map (\_ -> MarkdownBlock Poetry)


quotationBlock : Parser BlockType
quotationBlock =
    (succeed ()
        |. symbol (Token "> " (Expecting "expecting '> ' to begin quotation"))
    )
        |> map (\_ -> MarkdownBlock Quotation)


orderedListItemBlock : Parser BlockType
orderedListItemBlock =
    (succeed ()
        |. parseWhile (\c -> c == ' ')
        |. chompIf (\c -> Char.isDigit c) (Expecting "Expecting digit to begin ordered list item")
        |. chompWhile (\c -> Char.isDigit c)
        |. symbol (Token ". " (Expecting "expecting period"))
       ) |> map (\_ -> (MarkdownBlock OListItem))


horizontalRuleBlock : Parser BlockType
horizontalRuleBlock =
    (succeed ()
        |. spaces
        |. symbol (Token "___" (Expecting "Expecting at least three underscores to begin thematic break"))
    )
        |> map (\x -> MarkdownBlock HorizontalRule)


headingBlock : Parser BlockType
headingBlock =
    (succeed identity
        |. spaces
        |. symbol (Token "#" (Expecting "Expecting '#' to begin heading block"))
        |= parseWhile (\c -> c == '#')
    ) |> map (\s -> (MarkdownBlock (Heading ((String.length s) + 1))))


codeBlock : Parser BlockType
codeBlock =
    succeed (BalancedBlock DisplayCode)
        |. symbol (Token "```" (Expecting "Expecting four ticks to begin verbatim block"))


verbatimBlock : Parser BlockType
verbatimBlock =
    succeed (BalancedBlock Verbatim)
        |. symbol (Token "````" (Expecting "Expecting four ticks to begin verbatim block"))


mathBlock : Parser BlockType
mathBlock =
    succeed (BalancedBlock DisplayMath)
        |. symbol (Token "$$" (Expecting "Expecting four ticks to begin verbatim block"))


imageBlock : Parser BlockType
imageBlock =
    succeed (MarkdownBlock Image)
        |. symbol (Token "![" (Expecting "Expecting '![' to begin image block"))


unorderedListItemBlock : Parser BlockType
unorderedListItemBlock =
    succeed (MarkdownBlock UListItem)
        |. symbol (Token "- " (Expecting "Expecting '-' to begin list item"))


parseWhile : (Char -> Bool) -> Parser String
parseWhile accepting =
    chompWhile accepting |> getChompedString
