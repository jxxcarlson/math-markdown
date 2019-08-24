module MMInline exposing (..)

import Parser.Advanced exposing (..)


type alias Parser a =
    Parser.Advanced.Parser Context Problem a


type Context
    = Definition String
    | List
    | Record


type Problem
    = Expecting String



type MMInline
    = OrdinaryText String
    | ItalicText String
    | BoldText String
    | Code String
    | InlineMath String
    | StrikeThroughText String
    | BracketedText String
    | Link String String
    | MMInlineList (List MMInline)
    | Error (List MMInline)


type alias PrefixedString =
    { prefix : String, text : String }




{-|

> run (parseUntil ";;") "a b c;;"
> Ok ("a b c") : Result (List P.DeadEnd) String

-}
parseUntil : String -> Parser String
parseUntil end =
    chompUntil (Token end (Expecting <| "Expecting '" ++ end ++ "' in parseUntil")) |> getChompedString


parseWhile : (Char -> Bool) -> Parser String
parseWhile accepting =
    chompWhile accepting |> getChompedString



--
-- INLINE
--


{-|

> run ordinaryText "abc"
> Ok (OrdinaryText "abc")

-}
ordinaryText : Parser MMInline
ordinaryText =
    (succeed ()
        |. chompIf (\c -> not <| List.member c [ '`', '~', '[', '$', '*', '\n' ]) (Expecting "expecting regular character to begin ordinary text line")
        |. chompWhile (\c -> not <| List.member c [ '`', '~', '[', ']', '$', '*', '\n' ])
    )
        |> getChompedString
        |> map OrdinaryText


{-|

> run italicText "_abc_"
> Ok (ItalicText "abc")

-}
link : Parser MMInline
link =
    (succeed PrefixedString
        |. symbol (Token "[" (Expecting "expecting '[' to begin link"))
        |= parseWhile (\c -> c /= ']')
        |. symbol (Token "]" (Expecting "expecting ']' to end first part of link"))
        |= oneOf [ linkLabel, terminateBracket ]
        |. spaces
    )
        |> map (\ps -> linkOrBracket ps)


linkOrBracket : PrefixedString -> MMInline
linkOrBracket ps =
    case ps.text of
        " " ->
            BracketedText ps.prefix

        _ ->
            Link ps.text ps.prefix


linkLabel : Parser String
linkLabel =
    succeed identity
        |. symbol (Token "(" (Expecting "expecting '(' to begin link label"))
        |= parseWhile (\c -> c /= ')')
        |. symbol (Token ")" (Expecting "expecting ')' to end link label"))
        |. spaces


terminateBracket : Parser String
terminateBracket =
    (succeed ()
     -- |. symbol (Token " " DummyExpectation)
    )
        |> map (\_ -> " ")


strikeThroughText : Parser MMInline
strikeThroughText =
    (succeed ()
        |. symbol (Token "~~" (Expecting "expecting '~~' to begin strikethrough"))
        |. chompWhile (\c -> c /= '~')
        |. symbol (Token "~~" (Expecting "expecting '~~' to end strikethrough"))
        |. spaces
    )
        |> getChompedString
        |> map (String.dropLeft 2)
        |> map (String.replace "~~" "")
        |> map StrikeThroughText


boldText : Parser MMInline
boldText =
    (succeed ()
        |. symbol (Token "**" (Expecting "expecting '**' to begin bold text"))
        |. chompWhile (\c -> c /= '*')
        |. symbol (Token "**" (Expecting "expecting '**' to end bold text"))
        |. spaces
    )
        |> getChompedString
        |> map (String.dropLeft 2)
        |> map (String.replace "**" "")
        |> map BoldText


italicText : Parser MMInline
italicText =
    (succeed ()
        |. symbol (Token "*" (Expecting "Expecting '*' to begin italic text"))
        |. chompWhile (\c -> c /= '*')
        |. symbol (Token "*" (Expecting "Expecting '*' to end italic text"))
        |. spaces
    )
        |> getChompedString
        |> map (String.replace "*" "")
        |> map ItalicText



-- boldText : Parser MMInline
-- boldText =
--     (succeed ()
--         |. symbol "**"
--         |. chompWhile (\c -> c /= '*')
--         |. symbol "**"
--         |. spaces
--     )
--         |> getChompedString
--         |> map (String.dropLeft 2)
--         |> map (String.replace "**" "")
--         |> map BoldText


{-|

> run inlineMath "$a^5 = 3$"
> Ok (InlineMath ("a^5 = 3"))

-}
inlineMath : Parser MMInline
inlineMath =
    (succeed ()
        |. symbol (Token "$" (Expecting "Expecting '$' to begin inline math"))
        |. chompWhile (\c -> c /= '$')
        |. symbol (Token "$" (Expecting "Expecting '$' to end inline math"))
        |. chompWhile (\c -> c == ' ')
    )
        |> getChompedString
        |> map String.trim
        |> map (String.dropLeft 1)
        |> map (String.dropRight 1)
        |> map InlineMath


code : Parser MMInline
code =
    (succeed ()
        |. symbol (Token "`" (Expecting "Expecting '``' to begin inline code"))
        |. chompWhile (\c -> c /= '`')
        |. symbol (Token "`" (Expecting "Expecting '``' to end inline code"))
        |. chompWhile (\c -> c /= ' ')
    )
        |> getChompedString
        |> map String.trim
        |> map (String.dropLeft 1)
        |> map (String.dropRight 1)
        |> map Code


{-|

> run inline "$a^5 = 1$"
> Ok (InlineMath ("a^5 = 1"))

> run inline "_abc_"
> Ok (ItalicText "abc")

> run inline "hahaha"
> Ok (OrdinaryText "hahaha")

-}
inline : Parser MMInline
inline =
    oneOf [ code, link, boldText, italicText, strikeThroughText, inlineMath, ordinaryText ]


{-|

> run inlineList "_foo_ hahaha\\nnhohoh\\nn$a^6 + 2$"
> Ok [ItalicText ("foo "),OrdinaryText "hahaha",OrdinaryText "nhohoh",OrdinaryText "n",InlineMath ("a^6 + 2")]

-}
inlineList : Parser (List MMInline)
inlineList =
    many inline


runInlineList : String -> MMInline
runInlineList str =
    run inlineList str
        |> resolveInlineResult


resolveInlineResult : Result (List (DeadEnd Context Problem)) (List MMInline) -> MMInline
resolveInlineResult result =
    case result of
        Ok res_ ->
            res_ |> MMInlineList

        Err list ->
            decodeInlineError list


decodeInlineError : List (DeadEnd Context Problem) -> MMInline
decodeInlineError errorList =
    let
        errorMessage =
            List.map displayDeadEnd errorList
                |> String.join ";;\n\n"
    in
    OrdinaryText errorMessage


errorString : List (DeadEnd Context Problem) -> String
errorString errorList =
    List.map displayDeadEnd errorList
        |> String.join "\n"


displayDeadEnd : DeadEnd Context Problem -> String
displayDeadEnd deadend =
    case deadend.problem of
        Expecting error ->
            error



---
-- HELPERS
--


joinMMInlineLists : MMInline -> MMInline -> MMInline
joinMMInlineLists a b =
    case ( a, b ) of
        ( MMInlineList aList, MMInlineList bList ) ->
            MMInlineList (aList ++ bList)

        ( _, _ ) ->
            MMInlineList []


many : Parser a -> Parser (List a)
many p =
    loop [] (manyHelp p)


manyHelp : Parser a -> List a -> Parser (Step (List a) (List a))
manyHelp p vs =
    oneOf
        [ succeed (\v -> Loop (v :: vs))
            |= p
        , succeed ()
            |> map (\_ -> Done (List.reverse vs))
        ]
