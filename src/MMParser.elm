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
    , joinMMInlineLists
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
    | ExpectingItalicEndSymbol String
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
    | ExpectingImageBlockPrefix
    | ExpectingImageBlockInfix
    | ExpectingImageBlockSuffix
    | ExpectingUrlPrefix
    | ExpectingUrlSuffix
    | DummyExpectation
    | ExpectingQuotationStartSymbol String
    | Expecting String



--
-- AST
--


type MMBlock
    = MMList (List MMBlock)
    | ClosedBlock MMInline
    | ErrorBlock String
      --
    | Paragraph (List String)
    | HeadingBlock Int MMInline
    | MathDisplayBlock String
    | CodeBlock String
    | HorizontalRuleBlock
    | ListItemBlock Int MMInline
    | OrderedListItemBlock Int MMInline
    | ImageBlock String String
    | QuotationBlock MMInline
    | PoetryBlock MMInline


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


block =
    oneOf
        [ horizontalRuleBlock
        , poetryBlock
        , quotationBlock
        , imageBlock
        , backtrackable unorderedListItemBlock
        , backtrackable orderedListItemBlock
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

        Err error ->
            [ ErrorBlock (errorString error) ]


{-|

> closeBlock <| Paragraph ("_foo_ ha ha ha\\nho ho ho\\n$a^6 + 2$")
> ClosedBlock (MMInlineList [ItalicText ("foo "),OrdinaryText ("ha ha ha"),OrdinaryText ("ho ho ho"),InlineMath ("a^6 + 2")])

-}
closeBlock : MMBlock -> MMBlock
closeBlock block_ =
    case block_ of
        Paragraph stringList ->
            stringList |> String.join " " |> runInlineList |> ClosedBlock

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
                |. chompIf (\c -> not <| List.member c [ '`', '>', '!', '$', '#', '-', '\n' ]) (Expecting "Expecting line start")
                |. chompWhile (\c -> c /= '\n')
                |. symbol (Token "\n" (Expecting "Expecting line end"))


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
        |. symbol (Token "\n" (Expecting "Expecting end of paragraph as list"))
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
        |. symbol (Token "$$" (Expecting "Expecting begining $$"))
        |. chompWhile (\c -> c /= '$')
        |. symbol (Token "$$" (Expecting "Expecting ending $$"))
        |. symbol (Token "\n\n" (Expecting "Expecting blanklines at end of math display"))
        |. chompWhile (\c -> c == '\n')
    )
        |> getChompedString
        |> map String.trim
        |> map (String.dropLeft 2)
        |> map (String.dropRight 2)
        |> map MathDisplayBlock


quotationBlock : Parser MMBlock
quotationBlock =
    (succeed identity
        |. symbol (Token ">" (Expecting "expecting '>' to begin quotation"))
        |= paragraphAsList
    )
        |> map (String.join " ")
        |> map runInlineList
        |> map QuotationBlock


poetryBlock : Parser MMBlock
poetryBlock =
    (succeed identity
        |. symbol (Token ">>" (Expecting "expecting '>>' to begin poetry block"))
        |= paragraphAsList
    )
        |> map (List.map runInlineList)
        |> map MMInlineList
        |> map PoetryBlock


codeBlock : Parser MMBlock
codeBlock =
    (succeed ()
        |. symbol (Token "```" (Expecting "Expecting '```' to begin code block"))
        |. chompWhile (\c -> c /= '`')
        |. symbol (Token "```" (Expecting "Expecting '```' to end code block"))
        |. symbol (Token "\n\n" (Expecting "Expecting blank lines to end code block"))
        |. chompWhile (\c -> c == '\n')
    )
        |> getChompedString
        |> map String.trim
        |> map (String.dropLeft 3)
        |> map (String.dropRight 3)
        |> map String.trim
        |> map CodeBlock


imageBlock : Parser MMBlock
imageBlock =
    (succeed PrefixedString
        |. symbol (Token "![" (Expecting "Expecting '![' to begin image block"))
        |= parseWhile (\c -> c /= ']')
        |. symbol (Token "](" (Expecting "Expecting ']()' in image block"))
        |= parseWhile (\c -> c /= ')')
        |. symbol (Token ")\n\n" (Expecting "Expecting ')' plus blank lines to end image block"))
        |. chompWhile (\c -> c == '\n')
    )
        |> map (\ps -> ImageBlock ps.prefix ps.text)


type alias PrefixedString =
    { prefix : String, text : String }


{-|

> run headingBlock "### foo bar \\n\\n"
> Ok (HeadingBlock 3 ("foo bar"))

-}
headingBlock : Parser MMBlock
headingBlock =
    (succeed PrefixedString
        |. symbol (Token "#" (Expecting "Expecting '#' to begin heading block"))
        |= parseWhile (\c -> c == '#')
        -- |. symbol " "
        |= parseWhile (\c -> c /= '\n')
        |. symbol (Token "\n\n" (Expecting "Expecting blank lines to end heading block"))
        |. chompWhile (\c -> c == '\n')
    )
        |> map
            (\ps ->
                HeadingBlock
                    (String.length ps.prefix + 1)
                    (ps.text
                        |> String.replace "\\n\\n" ""
                        |> String.trim
                        |> runInlineList
                    )
            )


horizontalRuleBlock : Parser MMBlock
horizontalRuleBlock =
    (succeed ()
        |. symbol (Token "___" (Expecting "Expecting at least three underscores to begin thematic break"))
        |. parseWhile (\c -> c /= '\n')
        |. symbol (Token "\n\n" (Expecting "Expecting blank lines to end thematic break"))
        |. chompWhile (\c -> c == '\n')
    )
        |> map (\x -> HorizontalRuleBlock)


unorderedListItemBlock : Parser MMBlock
unorderedListItemBlock =
    (succeed PrefixedString
        |= parseWhile (\c -> c == ' ')
        |. symbol (Token "- " (Expecting "Expecting '-' to begin list item"))
        |= parseWhile (\c -> c /= '\n')
        |. symbol (Token "\n\n" (Expecting "Expecting blank lines to end list item"))
        |. chompWhile (\c -> c == '\n')
    )
        |> map
            (\ps ->
                ListItemBlock
                    (String.length ps.prefix // 4 + 1)
                    (ps.text
                        |> String.replace "\n\n" ""
                        |> String.trim
                        |> (\x -> x ++ "\n\n")
                        |> runInlineList
                    )
            )


orderedListItemBlock : Parser MMBlock
orderedListItemBlock =
    (succeed PrefixedString
        |= parseWhile (\c -> c == ' ')
        |. chompIf (\c -> Char.isDigit c) (Expecting "Expecting digit to begin ordered list item")
        |. chompWhile (\c -> Char.isDigit c)
        |. symbol (Token ". " (Expecting "expecting period"))
        |= parseWhile (\c -> c /= '\n')
        |. symbol (Token "\n\n" (Expecting "expecting blanks lines to end ordered list item"))
        |. chompWhile (\c -> c == '\n')
    )
        |> map
            (\ps ->
                OrderedListItemBlock
                    (String.length ps.prefix // 4 + 1)
                    (ps.text
                        |> String.replace "\n\n" ""
                        |> String.trim
                        |> (\x -> x ++ "\n\n")
                        |> runInlineList
                    )
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
        |= oneOf [ linkUrl, terminateBracket ]
        |. spaces
    )
        |> map (\ps -> linkOrBracket ps)


linkOrBracket : PrefixedString -> MMInline
linkOrBracket ps =
    case ps.text of
        " " ->
            BracketedText ps.prefix

        _ ->
            Link ps.prefix ps.text


linkUrl : Parser String
linkUrl =
    succeed identity
        |. symbol (Token "(" (Expecting "expecting '(' to begin url"))
        |= parseWhile (\c -> c /= ')')
        |. symbol (Token ")" (Expecting "expecting ')' to end url"))
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

        _ ->
            "(error)"



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
