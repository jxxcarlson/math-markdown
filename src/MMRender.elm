module MMRender exposing (render, renderBlock, renderClosedBlock)

import Html exposing (..)
import Html.Attributes as HA exposing (style)
import Json.Encode
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
            div [] (List.map renderBlock list)

        Paragraph stringList ->
            div [] (List.map text stringList)

        HeadingBlock level mmInlineList ->
            case level of
                1 ->
                    h1 [ style "font-size" "24pt" ] [ renderClosedBlock mmInlineList ]

                2 ->
                    h1 [ style "font-size" "18pt" ] [ renderClosedBlock mmInlineList ]

                3 ->
                    h3 [ style "font-size" "1 2pt" ] [ renderClosedBlock mmInlineList ]

                _ ->
                    h4 [] [ renderClosedBlock mmInlineList ]

        ImageBlock label url ->
            img [ HA.src url, style "width" "100%" ] [ text label ]

        MathDisplayBlock str ->
            displayMathText str

        -- div [] [ text <| "$$" ++ str ++ "$$" ]
        CodeBlock str ->
            pre [ style "font" "16px courier", style "padding" "12px", style "background-color" "#eee" ]
                [ code [] [ text str ] ]

        ListItemBlock k mmInline ->
            let
                margin =
                    String.fromInt (18 * k)
                        ++ "px"
            in
            li [ style "margin-left" margin ] [ renderClosedBlock mmInline ]

        QuotationBlock mmInline ->
            div
                [ style "margin-left" "24px"
                , style "margin-top" "18px"
                , style "margin-right" "36px"
                , style "margin-bottom" "18px"
                ]
                [ renderClosedBlock mmInline ]

        PoetryBlock mmInline ->
            div
                [ style "margin-left" "24px"
                , style "margin-top" "18px"
                , style "margin-right" "36px"
                , style "margin-bottom" "18px"
                ]
                [ renderClosedBlock mmInline ]

        HorizontalRuleBlock ->
            hr [] []

        ClosedBlock mmInline ->
            div [ style "margin-bottom" "12px" ] [ renderClosedBlock mmInline ]

        ErrorBlock errorMessage ->
            div [ style "color" "red", style "margin-bottom" "12px" ] [ text errorMessage ]


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
            span [ style "text-decoration" "line-through" ] [ text str ]

        BracketedText str ->
            span [] [ text <| "[" ++ str ++ "]" ]

        Code str ->
            span [ style "font" "16px courier", style "background-color" "#eee" ]
                [ code [] [ text (str ++ "!!") ]
                ]

        InlineMath str ->
            inlineMathText str

        MMInlineList list ->
            div [] (List.map renderClosedBlock list)

        Link url label ->
            a [ HA.href url ] [ text label ]

        Error _ ->
            div [] [ text "Error" ]



-- [ClosedBlock (MMInlineList [ItalicText ("foo "),OrdinaryText ("ha ha ha"),OrdinaryText ("ho ho ho"),InlineMath ("a^6 + 2")]),MathDisplayBlock ("a^2 = 3")]
--     : List MMBlock


mathText : String -> Html msg
mathText content =
    Html.node "math-text"
        [ HA.property "content" (Json.Encode.string content) ]
        []


inlineMathText : String -> Html msg
inlineMathText str =
    mathText <| "$ " ++ String.trim str ++ " $ "


displayMathText : String -> Html msg
displayMathText str =
    let
        str2 =
            String.trim str
    in
    mathText <| "$$\n" ++ str2 ++ "\n$$"
