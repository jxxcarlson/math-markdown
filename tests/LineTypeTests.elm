module LineTypeTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import LineType exposing(..)
import Parser.Advanced exposing(run)

suite : Test
suite =
    describe "The MMParser module"
        [ lineTypeTests

        ]



lineTypeTests =
    describe "LineType.parse"
        [ test "code block" <|
                \_ ->
                 "```foo" |> run parse |>  Expect.equal (Ok (BalancedBlock DisplayCode))

          , test "verbatim block" <|
                \_ ->
                 "````foo" |> run parse |>  Expect.equal (Ok (BalancedBlock Verbatim))

          , test "display math block" <|
                  \_ ->
                       "$$\n" |> run parse |>  Expect.equal (Ok (BalancedBlock DisplayMath))

         , test "image block" <|
                  \_ ->
                       "![yada]" |> run parse |>  Expect.equal (Ok (MarkdownBlock Image))

        , test "unordered list item k" <|
                  \_ ->
                       "- One]" |> run parse |>  Expect.equal (Ok (MarkdownBlock UListItem))

        ]
