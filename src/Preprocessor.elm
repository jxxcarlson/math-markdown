module Preprocessor exposing (fixBlockPrefix, startsBlock, transform)

import Regex


transform : String -> String
transform input =
    let
        isVerbatim =
            String.startsWith "```" input

        join : Bool -> List String -> String
        join bit stringList =
            case bit of
                True ->
                    String.join "\n" stringList

                False ->
                    String.join " " stringList
    in
    String.split "\n" input
        |> List.map fixBlockPrefix
        |> join isVerbatim


fixBlockPrefix : String -> String
fixBlockPrefix str =
    case startsBlock str of
        True ->
            "\n" ++ str

        False ->
            str


startsBlock : String -> Bool
startsBlock str =
    case Regex.find startsBlockRegex str |> List.head |> Maybe.map .index of
        Just 0 ->
            True

        _ ->
            False


startsBlockRegex : Regex.Regex
startsBlockRegex =
    Maybe.withDefault Regex.never <| Regex.fromString " *[->!\\[0123456789]"
