module MMRender exposing
    ( fixList
    , render
    , renderBlock
    , renderClosedBlock
    )

import Html exposing (..)
import Html.Attributes as HA exposing (style)
import Json.Encode
import MMAccumulator exposing (MMData, MMState)
import MMParser exposing (MMBlock(..), MMInline(..), joinMMInlineLists)


{-|

> runBlocks "~~Elimininate this~~, _please_\\n\\n" |> render
> <internals> : Html.Html msg

-}
render : List MMData -> List (Html msg)
render mmDataList =
    List.map renderBlock (List.concat <| fixList mmDataList)


fixList : List MMData -> List (List ( MMBlock, MMState ))
fixList mmDataList =
    List.map fix mmDataList



-- type alias MMData =
--     ( List MMBlock, MMState )


fix : MMData -> List ( MMBlock, MMState )
fix mmData =
    let
        ( blockList, mmState ) =
            mmData
    in
    List.map (\b -> ( b, mmState )) blockList


{-|

> runBlocks "~~Elimininate this~~, _please_\\n\\n" |> List.map renderBlock
> [<internals>] : List (Html.Html msg)

-}
renderBlock : ( MMBlock, MMState ) -> Html msg
renderBlock ( mmBlock, mmState ) =
    case mmBlock of
        MMList list ->
            div [] (List.map renderBlock <| fix ( list, mmState ))

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

        -- MMList blockList ->
        --     List.map (\b -> renderBlock ( b, mmState )) blockList
        --         |> div []
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

        OrderedListItemBlock k mmInline ->
            let
                margin =
                    String.fromInt (18 * k)
                        ++ "px"

                label =
                    String.fromInt mmState.itemIndex1 ++ ". "

                content =
                    joinMMInlineLists (MMInlineList [ OrdinaryText label ]) mmInline
            in
            li [ style "margin-left" margin, style "list-style" "none" ] [ renderClosedBlock content ]

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
