module Preprocessor exposing (fixBlockPrefix, startsBlock, transform)

import Regex


transform : String -> String
transform input =
    String.split "\n" input
        |> List.map fixBlockPrefix
        |> String.join " "


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
