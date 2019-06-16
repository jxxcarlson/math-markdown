module MMRender exposing (render, renderClosedBlock)

import Html exposing (..)
import Html.Attributes exposing (style)
import MMParser exposing (MMBlock(..), MMInline(..))


render : List MMBlock -> List (Html msg)
render blockList =
    List.map renderBlock blockList


renderBlock : MMBlock -> Html msg
renderBlock block_ =
    case block_ of
        MMList list ->
            div [] [ text "MMList" ]

        Block block__ ->
            div [] [ text "Block" ]

        RawBlock str ->
            div [] [ text str ]

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

        InlineMath str ->
            span [] [ text <| "$" ++ str ++ "$" ]

        MMInlineList list ->
            div [] [ text "MMInlineList" ]

        Error _ ->
            div [] [ text "Error" ]



-- [ClosedBlock (MMInlineList [ItalicText ("foo "),OrdinaryText ("ha ha ha"),OrdinaryText ("ho ho ho"),InlineMath ("a^6 + 2")]),MathDisplayBlock ("a^2 = 3")]
--     : List MMBlock
