module MMParser exposing
    ( MMBlock(..)
    , MMInline(..)
    , PrefixedString
    , blankLines
    , block
    , blocks
    , closeBlock
    , inline
    , inlineList
    , line
    , lines
    , paragraph
    , paragraphBlock
    , paragraphs
    , runBlocks
    , runInlineList
    )

import Parser.Advanced exposing (..)


type alias Parser a =
    Parser.Advanced.Parser Context Problem a


type Context
    = Definition String
    | List
    | Record


type Problem
    = BadIndent
    | BadKeyword String
    | ExpectingStrikeThroughSymbol
    | ExpectingListStartSymbol
    | ExpectingParagraphEnd
    | ExpectingEndString String
    | ExpectingOrdinaryTextPrefix
    | ExpectingMathDisplayStartSymbol
    | ExpectingMathDisplayEndSymbol
    | ExpectingLinkPrefix
    | ExpectingLinkInfix
    | ExpectingLinkSuffix
    | ExpectingLineStart
    | ExpectingLineEnd
    | ExpectingParagraphAsListEnd
    | ExpectingItalicBeginSymbol
    | ExpectingItalicEndSymbol
    | ExpectingInlineMathBeginSymbol
    | ExpectingInlineMathEndSymbol
    | ExpectingHeadingBeginSymbol
    | ExpectingHeadingEndSymbol
    | ExpectingBeginCodeBlockSymbol
    | ExpectingEndCodeBlockSymbol
    | ExpectingCodeBlockParagraphTerminator
    | ExpectingInlineCodeBeginSymbol
    | ExpectingInlineCodeEndSymbol
    | ExpectingBoldBeginSymbol
    | ExpectingBoldEndSymbol


type MMBlock
    = MMList (List MMBlock)
    | ClosedBlock MMInline
      --
    | Paragraph (List String)
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
        , paragraphBlock
        ]


{-|

> run rawBlocks "$$a^2 - 7$$\\n\\nho ho ho!!\\n\\n"
> Ok (MMList [MathDisplayBlock ("a^2 - 7"),Paragraph ("ho ho ho!!")])

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
    case run (many block) str of
        Ok list ->
            List.map closeBlock list

        Err _ ->
            [ ClosedBlock (OrdinaryText "Error") ]


{-|

> closeBlock <| Paragraph ("_foo_ ha ha ha\\nho ho ho\\n$a^6 + 2$")
> ClosedBlock (MMInlineList [ItalicText ("foo "),OrdinaryText ("ha ha ha"),OrdinaryText ("ho ho ho"),InlineMath ("a^6 + 2")])

-}
closeBlock : MMBlock -> MMBlock
closeBlock block_ =
    case block_ of
        Paragraph stringList ->
            List.map runInlineList stringList |> MMInlineList |> ClosedBlock

        _ ->
            block_


{-|

> run paragraphBlock "a b c\\n\\n"
> Ok (Paragraph ("a b c"))

-}
paragraphBlock : Parser MMBlock
paragraphBlock =
    paragraphAsList
        |> map Paragraph


line : Parser String
line =
    map String.trim <|
        getChompedString <|
            succeed ()
                -- |. chompIf Char.isAlphaNum
                |. chompIf (\c -> not <| List.member c [ '$', '#', '-', '\n' ]) ExpectingLineStart
                |. chompWhile (\c -> c /= '\n')
                |. symbol (Token "\n" ExpectingLineEnd)


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
        |. symbol (Token "\n" ExpectingParagraphAsListEnd)
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
        |. symbol (Token "$$" ExpectingMathDisplayStartSymbol)
        |. chompWhile (\c -> c /= '$')
        |. symbol (Token "$$" ExpectingMathDisplayEndSymbol)
        |. symbol (Token "\n\n" ExpectingParagraphEnd)
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
        |. symbol (Token "```" ExpectingBeginCodeBlockSymbol)
        |. chompWhile (\c -> c /= '`')
        |. symbol (Token "```" ExpectingEndCodeBlockSymbol)
        |. symbol (Token "\n\n" ExpectingCodeBlockParagraphTerminator)
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
        |. symbol (Token "#" ExpectingHeadingBeginSymbol)
        |= parseWhile (\c -> c == '#')
        -- |. symbol " "
        |= parseWhile (\c -> c /= '\n')
        |. symbol (Token "\n\n" ExpectingHeadingEndSymbol)
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
        |. symbol (Token "- " ExpectingListStartSymbol)
        |= parseWhile (\c -> c /= '\n')
        |. symbol (Token "\n\n" ExpectingParagraphEnd)
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
    chompUntil (Token end (ExpectingEndString end)) |> getChompedString


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
        |. chompIf (\c -> not <| List.member c [ '$', '*', '\n' ]) ExpectingOrdinaryTextPrefix
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
        |. symbol (Token "[" ExpectingLinkPrefix)
        |= parseWhile (\c -> c /= ']')
        |. symbol (Token "](" ExpectingLinkInfix)
        |= parseWhile (\c -> c /= ')')
        |. symbol (Token ")" ExpectingLinkSuffix)
        |. spaces
    )
        |> map (\ps -> Link ps.prefix ps.text)


strikeThroughText : Parser MMInline
strikeThroughText =
    (succeed ()
        |. symbol (Token "~~" ExpectingStrikeThroughSymbol)
        |. chompWhile (\c -> c /= '~')
        |. symbol (Token "~~" ExpectingStrikeThroughSymbol)
        |. spaces
    )
        |> getChompedString
        |> map (String.dropLeft 2)
        |> map (String.replace "~~" "")
        |> map StrikeThroughText


boldText : Parser MMInline
boldText =
    (succeed ()
        |. symbol (Token "**" ExpectingBoldBeginSymbol)
        |. chompWhile (\c -> c /= '*')
        |. symbol (Token "**" ExpectingBoldEndSymbol)
        |. spaces
    )
        |> getChompedString
        |> map (String.dropLeft 2)
        |> map (String.replace "**" "")
        |> map BoldText


italicText : Parser MMInline
italicText =
    (succeed ()
        |. symbol (Token "*" ExpectingItalicBeginSymbol)
        |. chompWhile (\c -> c /= '*')
        |. symbol (Token "*" ExpectingItalicEndSymbol)
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
        |. symbol (Token "`" ExpectingInlineCodeBeginSymbol)
        |. chompWhile (\c -> c /= '`')
        |. symbol (Token "`" ExpectingInlineCodeEndSymbol)
        |. chompWhile (\c -> c /= ' ')
    )
        |> getChompedString
        |> map String.trim
        |> map (String.dropLeft 1)
        |> map (String.dropRight 1)
        |> map Code


{-|

> run inlineMath "$a^5 = 3$"
> Ok (InlineMath ("a^5 = 3"))

-}
inlineMath : Parser MMInline
inlineMath =
    (succeed ()
        |. symbol (Token "$" ExpectingInlineMathBeginSymbol)
        |. chompWhile (\c -> c /= '$')
        |. symbol (Token "$" ExpectingInlineMathEndSymbol)
        |. chompWhile (\c -> c == ' ')
    )
        |> getChompedString
        |> map String.trim
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


resolveInlineResult : Result (List (DeadEnd Context Problem)) (List MMInline) -> MMInline
resolveInlineResult result =
    case result of
        Ok res_ ->
            res_ |> MMInlineList

        Err list ->
            decodeInlineError list


decodeInlineError : List (DeadEnd Context Problem) -> MMInline
decodeInlineError list =
    OrdinaryText "error"



---
-- HELPERS
--


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
