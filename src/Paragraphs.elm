module Paragraphs exposing (parse)

{-| This module exports just one function,
intended to turn a string into a lisst
of logical paragraphs. It operates as a
finite-state machine.


# API

@docs logicalParagraphify

-}

import Regex


type ParserState
    = Start
    | InParagraph
    | IgnoreLine
    | Error


type LineType
    = Blank
    | Ignore
    | Text


type alias ParserRecord =
    { currentParagraph : String
    , paragraphList : List String
    , state : ParserState
    }


lineType : String -> LineType
lineType line =
    if line == "" then
        Blank

    else
        Text


{-| getNextState is the transition function for a finite-state
machine which parses lines.
-}
getNextState : String -> ParserState -> ParserState
getNextState line parserState =
    case ( parserState, lineType line ) of
        ( Start, Blank ) ->
            Start

        ( Start, Text ) ->
            InParagraph

        ( Start, Ignore ) ->
            IgnoreLine

        ( IgnoreLine, Blank ) ->
            Start

        ( IgnoreLine, Text ) ->
            InParagraph

        ( InParagraph, Text ) ->
            InParagraph

        ( InParagraph, Blank ) ->
            Start

        ( _, _ ) ->
            Error


joinLines : String -> String -> String
joinLines a b =
    case ( a, b ) of
        ( "", _ ) ->
            b

        ( _, "" ) ->
            a

        ( "\n", _ ) ->
            "\n" ++ b

        ( _, "\n" ) ->
            a ++ "\n"

        ( aa, bb ) ->
            aa ++ "\n" ++ bb


fixLine : String -> String
fixLine line =
    if line == "" then
        "\n"

    else
        line


{-| Given an line and a parserRecord, compute a new parserRecord.
The new parserRecord depends on the getNextState of the FSM. Note
that the state of the machine is part of the parserRecord.
-}
updateParserRecord : String -> ParserRecord -> ParserRecord
updateParserRecord line parserRecord =
    let
        nextState =
            getNextState line parserRecord.state
    in
    case nextState of
        Start ->
            { parserRecord
                | currentParagraph = ""
                , paragraphList = parserRecord.paragraphList ++ [ joinLines parserRecord.currentParagraph line ]
                , state = nextState
            }

        InParagraph ->
            { parserRecord
                | currentParagraph = joinLines parserRecord.currentParagraph line
                , state = nextState
            }

        IgnoreLine ->
            parserRecord

        Error ->
            parserRecord


logicalParagraphParse : String -> ParserRecord
logicalParagraphParse text =
    (text ++ "\n")
        |> String.split "\n"
        |> List.foldl updateParserRecord { currentParagraph = "", paragraphList = [], state = Start }


{-| logicalParagraphify text: split text into logical
parapgraphs, where these are either normal paragraphs, i.e.,
blocks text with no blank lines surrounded by blank lines,
or outer blocks of the form \\begin{_} ... \\end{_}.
-}
logicalParagraphify : String -> List String
logicalParagraphify text =
    let
        lastState =
            logicalParagraphParse text
    in
    lastState.paragraphList
        ++ [ lastState.currentParagraph ]
        |> List.filter (\x -> x /= "")
        |> List.map (\paragraph -> String.trim paragraph ++ "\n\n")


para : Regex.Regex
para =
    Maybe.withDefault Regex.never <|
        Regex.fromString "\\n\\n+"


parse : String -> List String
parse text =
    --String.split "\n\n" text
    Regex.split para text
        |> List.filter (\x -> String.length x /= 0)
        |> List.map (\x -> x ++ "\n\n")
