module MMRender exposing (render, renderBlock, renderClosedBlock)

import Html exposing (..)
import Html.Attributes exposing (style)
import MMParser exposing (MMBlock(..), MMInline(..))


{-|

> runBlocks "~~Elimininate this~~, _please_\\n\\n" |> render
> <internals> : Html.Html msg

-}
render : List (Html.Attribute msg) -> List MMBlock -> Html msg
render attrList blockList =
    List.map renderBlock blockList
        |> (\stuff -> div attrList stuff)


{-|

> runBlocks "~~Elimininate this~~, _please_\\n\\n" |> List.map renderBlock
> [<internals>] : List (Html.Html msg)

-}
renderBlock : MMBlock -> Html msg
renderBlock block_ =
    case block_ of
        MMList list ->
            div [] [ text "MMList" ]

        Block block__ ->
            div [] [ text "Block" ]

        RawBlock str ->
            div [] [ text str ]

        HeadingBlock level str ->
            case level of
                1 ->
                    h1 [] [ text str ]

                _ ->
                    h5 [] [ text str ]

        MathDisplayBlock str ->
            div [] [ text <| "$$" ++ str ++ "$$" ]

        ClosedBlock mmInline ->
            renderClosedBlock mmInline


renderClosedBlock : MMInline -> Html msg
renderClosedBlock mmInline =
    case mmInline of
        OrdinaryText str ->
            span [] [ text str ]

        ItalicText str ->
            em [] [ text str ]

        BoldText str ->
            strong [] [ text str ]

        StrikeThroughText str ->
            strong [] [ text str ]

        InlineMath str ->
            span [] [ text <| "$" ++ str ++ "$" ]

        MMInlineList list ->
            div [] [ text "MMInlineList" ]

        Error _ ->
            div [] [ text "Error" ]



-- [ClosedBlock (MMInlineList [ItalicText ("foo "),OrdinaryText ("ha ha ha"),OrdinaryText ("ho ho ho"),InlineMath ("a^6 + 2")]),MathDisplayBlock ("a^2 = 3")]
--     : List MMBlock
