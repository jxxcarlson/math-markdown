module MMarkdown exposing (toHtml)

-- import Html.Attributes as HA exposing (Attribute)

import Html exposing (Html)
import MMParser
import MMRender
import Paragraphs


toHtml : List (Html.Attribute msg) -> String -> Html msg
toHtml attrList str =
    str
        |> Paragraphs.parse
        |> List.map MMParser.runBlocks
        |> List.map (MMRender.render attrList)
        |> Html.div []
