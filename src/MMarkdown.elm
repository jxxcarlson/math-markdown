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
import MMParser exposing (MMBlock)
import MMRender exposing (fixList)
import Paragraphs


{-| MMarkdown.toHtml renders a string of Markdown text to Html
-}
toHtml : List (Html.Attribute msg) -> String -> Html msg
toHtml attrList str =
    let
        _ =
            Debug.log "HERE I AM!"
    in
    str
        |> Paragraphs.parse
        |> MMAccumulator.parse MMAccumulator.emptyMMState
        |> MMRender.render attrList
        |> (\x -> Html.div [] [ x ])
