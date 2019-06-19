module ParserTests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import MMParser exposing (..)
import Parser.Advanced exposing (run)
import Test exposing (..)


suite : Test
suite =
    describe "The MMParser module"
        [ lineTests
        , paragraphTests
        , blockParserTests
        , listTests
        , inlineParserTests
        , runBlocksTest
        ]


lineTests =
    describe "lines"
        [ test "line" <|
            \_ ->
                "ho ho ho\n"
                    |> run line
                    |> Expect.equal (Ok "ho ho ho")
        , test "line (fail)" <|
            \_ ->
                "ho ho ho"
                    |> run line
                    |> Expect.notEqual (Ok "ho ho ho")
        , test "many lines" <|
            \_ ->
                "a\nb\nc\n"
                    |> run lines
                    |> Expect.equal (Ok [ "a", "b", "c" ])
        , test "many lines (fail)" <|
            \_ ->
                "a\n\nb\nc\n"
                    |> run lines
                    |> Expect.notEqual (Ok [ "a", "b", "c" ])
        , test "blankLines" <|
            \_ ->
                "\n\n\n"
                    |> run blankLines
                    |> Expect.equal (Ok ())
        , test "blankLines 2" <|
            \_ ->
                ""
                    |> run blankLines
                    |> Expect.equal (Ok ())
        ]


paragraphTests =
    describe "paragraphs"
        [ test "one paragraph" <|
            \_ ->
                "line1\nline2\nline3\n\n"
                    |> run paragraph
                    |> Expect.equal (Ok "line1\nline2\nline3")
        , test "two paragraphs" <|
            \_ ->
                "line1\nline2\nline3\n\nx\ny\nz\n\n"
                    |> run paragraphs
                    |> Expect.equal (Ok [ "line1\nline2\nline3", "x\ny\nz" ])
        ]


special =
    describe "block parsers"
        [ test "listtBlocks12" <|
            \_ ->
                "- Solids\n\n    - Iron\n\n"
                    |> run blocks
                    |> Expect.equal (Ok (MMList [ ListItemBlock 1 "Solids", ListItemBlock 2 "Bacon", ListItemBlock 2 "Eggs" ]))
        ]


blockParserTests =
    describe "block parsers"
        [ test "headingBlock" <|
            \_ ->
                "# Tests \n\n"
                    |> run block
                    |> Expect.equal (Ok (HeadingBlock 1 "Tests"))
        , test "headingBlock (fail)" <|
            \_ ->
                "# Tests \n"
                    |> run block
                    |> Expect.notEqual (Ok (HeadingBlock 1 "Tests"))
        , test "mathBlock" <|
            \_ ->
                "$$a^2 - 7$$\n\n"
                    |> run block
                    |> Expect.equal (Ok (MathDisplayBlock "a^2 - 7"))
        , test "codeBlock" <|
            \_ ->
                "$$a^2 - 7$$\n\n"
                    |> run block
                    |> Expect.equal (Ok (MathDisplayBlock "a^2 - 7"))
        , test "block" <|
            \_ ->
                "```\nfoo == bar\n```\n\n"
                    |> run block
                    |> Expect.equal (Ok (CodeBlock "foo == bar"))
        , test "paragraphBlock" <|
            \_ ->
                "a b c\n\n"
                    |> run block
                    |> Expect.equal (Ok (Paragraph [ "a b c" ]))
        ]


listTests =
    describe "lists"
        [ test "listBlock1" <|
            \_ ->
                "- Solids\n\n"
                    |> run block
                    |> Expect.equal (Ok (ListItemBlock 1 "Solids"))
        , test "listBlock2" <|
            \_ ->
                "    - Iron\n\n"
                    |> run block
                    |> Expect.equal (Ok (ListItemBlock 2 "Iron"))
        , test "listBlock2b" <|
            \_ ->
                "    - Iron\n\n"
                    |> run blocks
                    |> Expect.equal (Ok (MMList [ ListItemBlock 2 "Iron" ]))
        , test "listBlock2c" <|
            \_ ->
                "ho ho ho\n\n    - Iron\n\n"
                    |> run blocks
                    |> Expect.equal (Ok (MMList [ Paragraph [ "ho ho ho" ], ListItemBlock 2 "Iron" ]))
        , test "listBlocks12" <|
            \_ ->
                "- Solids\n\n    - Iron\n\n"
                    |> run blocks
                    |> Expect.equal (Ok (MMList [ ListItemBlock 1 "Solids", ListItemBlock 2 "Iron" ]))
        , test "listBlocks" <|
            \_ ->
                listString
                    |> run blocks
                    |> Expect.equal (Ok (MMList [ ListItemBlock 1 "Solids", ListItemBlock 2 "Bacon", ListItemBlock 2 "Eggs" ]))
        ]


inlineParserTests =
    describe "inline parsers"
        [ test "italicText" <|
            \_ ->
                "*foo* ha ha ha"
                    |> run inline
                    |> Expect.equal (Ok (ItalicText "foo "))

        -- Expect.equal is designed to be used in pipeline style, like this.
        , test "boldText" <|
            \_ ->
                "**foo** ha ha ha"
                    |> run inline
                    |> Expect.equal (Ok (BoldText "foo "))
        , test "strikeThroughText" <|
            \_ ->
                "~~foo~~ ha ha ha"
                    |> run inline
                    |> Expect.equal (Ok (StrikeThroughText "foo "))
        , test "code" <|
            \_ ->
                "`foo := bar`"
                    |> run inline
                    |> Expect.equal (Ok (Code "foo := bar"))
        , test "runInlineList" <|
            \_ ->
                "*italic* **bold** ha ha ha $a^6 + 2$"
                    |> runInlineList
                    |> Expect.equal
                        (MMInlineList
                            [ ItalicText "italic "
                            , BoldText "bold "
                            , OrdinaryText "ha ha ha "
                            , InlineMath "a^6 + 2"
                            ]
                        )
        , test "links" <|
            \_ ->
                "[http://nytimes.com](New York Times)"
                    |> run inline
                    |> Expect.equal (Ok (Link "http://nytimes.com" "New York Times"))
        , test "bracketedText" <|
            \_ ->
                "a [b] c"
                    |> runInlineList
                    |> Expect.equal (MMInlineList [ OrdinaryText "a ", BracketedText "b", OrdinaryText "c" ])
        , test "bracketedText 2" <|
            \_ ->
                "a [b]c"
                    |> runInlineList
                    |> Expect.equal (MMInlineList [ OrdinaryText "a ", BracketedText "b", OrdinaryText "c" ])
        ]


runBlocksTest =
    describe "runBlocks"
        [ test "simple runBlocks" <|
            \_ ->
                "a\n\nb\n\n"
                    |> runBlocks
                    |> Expect.equal [ ClosedBlock (MMInlineList [ OrdinaryText "a" ]), ClosedBlock (MMInlineList [ OrdinaryText "b" ]) ]
        , test "simple runBlocks II" <|
            \_ ->
                "a *b* c\n\n"
                    |> runBlocks
                    |> Expect.equal [ ClosedBlock (MMInlineList [ OrdinaryText "a ", ItalicText "b ", OrdinaryText "c" ]) ]
        , test "two-line paragraph I" <|
            \_ ->
                "This is a test\nand so is this\n\n"
                    |> runBlocks
                    |> Expect.equal [ ClosedBlock (MMInlineList [ OrdinaryText "This is a test and so is this" ]) ]
        , test "two-line paragraph II" <|
            \_ ->
                "This is a test\nand so is this\n\n"
                    |> run blocks
                    |> Expect.equal (Ok (MMList [ Paragraph [ "This is a test", "and so is this" ] ]))
        ]


listString =
    "- Solids\n\n    - Bacon\n\n    - Eggs\n\n"


listString2 =
    """
- Solids

    - Bacon

    - Eggs

- Liquids

    - Orange Juice

    - Beer


"""


compactList =
    """

- Solids
- Liquids
- Gases


"""
