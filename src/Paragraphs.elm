module Paragraphs exposing (parse)

{-| This module exports just one function,
intended to turn a string into a lisst
of logical paragraphs. It operates as a
finite-state machine.


# API

@docs logicalParagraphify

-}

import MMParser
import Regex


type ParagraphType
    = TextType
    | VerbatimType


type ParserState
    = Start
    | InParagraph ParagraphType
    | IgnoreLine
    | Error


type LineType
    = Blank
    | Ignore
    | Text
    | VerbatimText


type alias ParserRecord =
    { currentParagraph : String
    , paragraphList : List String
    , state : ParserState
    }


parse2 : String -> List String
parse2 text =
    text
        |> String.lines


lineType : String -> LineType
lineType line =
    if MMParser.isBlankLine line then
        Blank

    else if String.startsWith "````" line then
        VerbatimText

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
            InParagraph TextType

        ( Start, VerbatimText ) ->
            InParagraph VerbatimType

        ( Start, Ignore ) ->
            IgnoreLine

        ---
        ( IgnoreLine, Blank ) ->
            Start

        ( IgnoreLine, Text ) ->
            InParagraph TextType

        ---
        ( InParagraph TextType, Text ) ->
            InParagraph TextType

        ---
        ( InParagraph VerbatimType, Text ) ->
            InParagraph VerbatimType

        ( InParagraph VerbatimType, Blank ) ->
            InParagraph VerbatimType

        ( InParagraph VerbatimType, VerbatimText ) ->
            Start

        ( InParagraph _, Blank ) ->
            Start

        ---
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

        InParagraph TextType ->
            { parserRecord
                | currentParagraph = joinLines parserRecord.currentParagraph line
                , state = nextState
            }

        InParagraph VerbatimType ->
            let
                line_ =
                    if line == "" then
                        "\n"

                    else
                        line
            in
            { parserRecord
                | currentParagraph = joinLines parserRecord.currentParagraph line_
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
parse : String -> List String
parse text =
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


regexBlank : Regex.Regex
regexBlank =
    Maybe.withDefault Regex.never <|
        Regex.fromString " *"
