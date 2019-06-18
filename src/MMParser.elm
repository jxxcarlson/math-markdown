module MMParser exposing
    ( MMBlock(..)
    , MMInline(..)
    , PrefixedString
    , blankLines
    , block
    , blocks
    , inline
    , inlineList
    , line
    , lines
    , many
    , paragraph
    , paragraphs
    , rawTextBlock
    , runBlocks
    , runInlineList
    )

import Parser exposing (..)


type MMBlock
    = MMList (List MMBlock)
    | Block MMBlock
    | ClosedBlock MMInline
      --
    | RawBlock String
    | HeadingBlock Int String
    | MathDisplayBlock String
    | CodeBlock String
    | ListItemBlock Int String


type MMInline
    = OrdinaryText String
    | ItalicText String
    | BoldText String
    | Code String
    | InlineMath String
    | StrikeThroughText String
    | Link String String
    | MMInlineList (List MMInline)
    | Error (List MMInline)


block =
    oneOf
        [ unorderedListItemBlock
        , headingBlock
        , codeBlock
        , mathBlock
        , rawTextBlock

        -- , blankParagraph
        ]


{-|

> run rawBlocks "$$a^2 - 7$$\\n\\nho ho ho!!\\n\\n"
> Ok (MMList [MathDisplayBlock ("a^2 - 7"),RawBlock ("ho ho ho!!")])

-}
blocks : Parser MMBlock
blocks =
    many block
        |> map MMList


{-|

> str
> "_foo_ ha ha ha\\nho ho ho\\n$a^6 + 2$\\n\\n$$a^2 = 3$$\\n\\n" : String

> runBlocks str
> [ClosedBlock (MMInlineList [ItalicText ("foo "),OrdinaryText ("ha ha ha"),OrdinaryText ("ho ho ho"),InlineMath ("a^6 + 2")]),MathDisplayBlock ("a^2 = 3")]

    : List MMBlock

-}
runBlocks : String -> List MMBlock
runBlocks str =
    case Parser.run (many block) str of
        Ok list ->
            List.map closeBlock list

        Err _ ->
            [ ClosedBlock (OrdinaryText "Error") ]


{-|

> closeBlock <| RawBlock ("_foo_ ha ha ha\\nho ho ho\\n$a^6 + 2$")
> ClosedBlock (MMInlineList [ItalicText ("foo "),OrdinaryText ("ha ha ha"),OrdinaryText ("ho ho ho"),InlineMath ("a^6 + 2")])

-}
closeBlock : MMBlock -> MMBlock
closeBlock block_ =
    case block_ of
        RawBlock str ->
            runInlineList str |> ClosedBlock

        _ ->
            block_


{-|

> run rawTextBlock "a b c\\n\\n"
> Ok (RawBlock ("a b c"))

-}
rawTextBlock1 : Parser MMBlock
rawTextBlock1 =
    (succeed identity
        |= parseWhile (\c -> c /= '\n')
        |. chompIf (\c -> c == '\n')
        |. chompIf (\c -> c == '\n')
        |. chompWhile (\c -> c == '\n')
    )
        |> map String.trim
        |> map RawBlock


rawTextBlock : Parser MMBlock
rawTextBlock =
    paragraph
        |> map RawBlock


line : Parser String
line =
    map String.trim <|
        getChompedString <|
            succeed ()
                -- |. chompIf Char.isAlphaNum
                |. chompIf (\c -> not <| List.member c [ '[', '$', '#', '-', '\n' ])
                |. chompWhile (\c -> c /= '\n')
                |. symbol "\n"


blankLines : Parser ()
blankLines =
    chompWhile (\c -> c == '\n')


lines : Parser (List String)
lines =
    many line


paragraph : Parser String
paragraph =
    paragraphAsList
        |> map (String.join "\n")


paragraphAsList : Parser (List String)
paragraphAsList =
    succeed identity
        |= lines
        |. symbol "\n"
        |. chompWhile (\c -> c == '\n')


paragraphs : Parser (List String)
paragraphs =
    many paragraph


{-|

> run mathBlock "$$a^2 - 7$$\\n\\n"
> Ok (MathDisplayBlock ("a^2 - 7"))

-}
mathBlock : Parser MMBlock
mathBlock =
    (succeed ()
        |. symbol "$$"
        |. chompWhile (\c -> c /= '$')
        |. symbol "$$"
        |. symbol "\n\n"
        |. chompWhile (\c -> c == '\n')
    )
        |> getChompedString
        |> map String.trim
        |> map (String.dropLeft 2)
        |> map (String.dropRight 2)
        |> map MathDisplayBlock


codeBlock : Parser MMBlock
codeBlock =
    (succeed ()
        |. symbol "```"
        |. chompWhile (\c -> c /= '`')
        |. symbol "```"
        |. symbol "\n\n"
        |. chompWhile (\c -> c == '\n')
    )
        |> getChompedString
        |> map String.trim
        |> map (String.dropLeft 3)
        |> map (String.dropRight 3)
        |> map String.trim
        |> map CodeBlock


type alias PrefixedString =
    { prefix : String, text : String }


{-|

> run headingBlock "### foo bar \\n\\n"
> Ok (HeadingBlock 3 ("foo bar"))

-}
headingBlock : Parser MMBlock
headingBlock =
    (succeed PrefixedString
        |. symbol "#"
        |= parseWhile (\c -> c == '#')
        -- |. symbol " "
        |= parseWhile (\c -> c /= '\n')
        |. symbol "\n\n"
        |. chompWhile (\c -> c == '\n')
    )
        |> map
            (\ps ->
                HeadingBlock
                    (String.length ps.prefix + 1)
                    (ps.text |> String.replace "\\n\\n" "" |> String.trim)
            )


unorderedListItemBlock : Parser MMBlock
unorderedListItemBlock =
    (succeed PrefixedString
        |= parseWhile (\c -> c == ' ')
        |. symbol "- "
        |= parseWhile (\c -> c /= '\n')
        |. symbol "\n"
        |. symbol "\n"
        |. chompWhile (\c -> c == '\n')
    )
        |> map
            (\ps ->
                ListItemBlock
                    ((modBy 3 <| String.length ps.prefix) + 1)
                    (ps.text |> String.replace "\\n\\n" "" |> String.trim)
            )



--
-- HELPERS
--


{-|

> run (parseUntil ";;") "a b c;;"
> Ok ("a b c") : Result (List P.DeadEnd) String

-}
parseUntil : String -> Parser String
parseUntil end =
    chompUntil end |> getChompedString


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
        |. chompIf (\c -> not <| List.member c [ '$', '*', '\n' ])
        |. chompWhile (\c -> not <| List.member c [ '$', '*', '\n' ])
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
        |. symbol "["
        |= parseWhile (\c -> c /= ']')
        |. symbol "]("
        |= parseWhile (\c -> c /= ')')
        |. symbol ")"
        |. spaces
    )
        |> map (\ps -> Link ps.prefix ps.text)


strikeThroughText : Parser MMInline
strikeThroughText =
    (succeed ()
        |. symbol "~~"
        |. chompWhile (\c -> c /= '~')
        |. symbol "~~"
        |. spaces
    )
        |> getChompedString
        |> map (String.dropLeft 2)
        |> map (String.replace "~~" "")
        |> map StrikeThroughText


boldText : Parser MMInline
boldText =
    (succeed ()
        |. symbol "**"
        |. chompWhile (\c -> c /= '*')
        |. symbol "**"
        |. spaces
    )
        |> getChompedString
        |> map (String.dropLeft 2)
        |> map (String.replace "**" "")
        |> map BoldText


italicText : Parser MMInline
italicText =
    (succeed ()
        |. symbol "*"
        |. chompWhile (\c -> c /= '*')
        |. symbol "*"
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


code : Parser MMInline
code =
    (succeed ()
        |. symbol "`"
        |. chompWhile (\c -> c /= '`')
        |. symbol "`"
        |. spaces
    )
        |> getChompedString
        |> map (String.dropLeft 1)
        |> map (String.replace "`" "")
        |> map Code


{-|

> run inlineMath "$a^5 = 3$"
> Ok (InlineMath ("a^5 = 3"))

-}
inlineMath : Parser MMInline
inlineMath =
    (succeed ()
        |. symbol "$"
        |. chompWhile (\c -> c /= '$')
        |. symbol "$"
        |. spaces
    )
        |> getChompedString
        |> map (String.dropLeft 1)
        |> map (String.dropRight 1)
        |> map InlineMath


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
    oneOf [ link, code, boldText, italicText, strikeThroughText, inlineMath, ordinaryText ]


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


resolveInlineResult : Result (List DeadEnd) (List MMInline) -> MMInline
resolveInlineResult result =
    case result of
        Ok res_ ->
            res_ |> MMInlineList

        Err err ->
            List.map decodeInlineError err
                |> MMInlineList


decodeInlineError : DeadEnd -> MMInline
decodeInlineError err =
    OrdinaryText "error"



--
-- TOOLS
--


{-| Apply a parser zero or more times and return a list of the results.
-}
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
