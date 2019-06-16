module MMarkdown exposing (toHtml)

-- import Html.Attributes as HA exposing (Attribute)

import Html exposing (Html)
import MMParser
import MMRender


toHtml : List (Html.Attribute msg) -> String -> Html msg
toHtml attrList str =
    MMParser.runBlocks str |> MMRender.render attrList
