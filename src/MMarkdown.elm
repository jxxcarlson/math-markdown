module MMarkdown exposing (toHtml)

{-| MMarkdown is an experimental package for rendering
Markdown that contains mathematical text. It uses MathJax
for the math text. See `index.html` for the needed MathJax setup.

At the present time, the version of Markdown is quite limited,
albeit serviceable. See XXX for a demo.

@docs toHtml

-}

import Html exposing (Html)
import Html.Attributes as HA
import MMAccumulator exposing (MMData, MMState)
import MMParagraphs
import MMParser exposing (MMBlock)
import MMRender exposing (fixList)


{-| MMarkdown.toHtml renders a string of Markdown text to Html
-}
toHtml : List (Html.Attribute msg) -> String -> Html msg
toHtml attrList str =
    str
        |> MMParagraphs.parse
        |> MMAccumulator.parse MMAccumulator.emptyMMState
        |> MMRender.render
        |> (\x -> Html.div attrList x)
