module MMarkdown exposing
    ( toHtml
    -- , parse
    )

{-| MMarkdown is an experimental package for rendering
Markdown that contains mathematical text. It uses MathJax
for the math text. See `index.html` in `./app-math`
for the needed MathJax setup.

At the present time, the version of Markdown is limited,
albeit serviceable. See the demo at https://markdown.minilatex.app/ .

@docs toHtml

-}

import Html exposing (Html)
import MMAccumulator exposing (MMData, MMState)
import MMParagraphs
import MMParser exposing (MMBlock)
import MMRender
import Preprocessor


{-| MMarkdown.toHtml renders a string of Markdown text to Html
-}
toHtml : List (Html.Attribute msg) -> String -> Html msg
toHtml attrList str =
    str
        |> Preprocessor.transform
        |> MMParagraphs.parse
        |> MMAccumulator.parse MMAccumulator.emptyMMState
        |> MMRender.render
        |> (\x -> Html.div attrList x)


parse : String -> List MMBlock
parse str =
    str
        --|> Preprocessor.transform
        |> MMParagraphs.parse
        |> MMAccumulator.parse MMAccumulator.emptyMMState
        |> List.map Tuple.first
        |> List.concat
