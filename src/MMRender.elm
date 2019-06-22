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
            div [ HA.class "mm-list" ] (List.map renderBlock <| fix ( list, mmState ))

        Paragraph stringList ->
            div [ HA.class "mm-paragraph" ] (List.map text stringList)

        HeadingBlock level mmInlineList ->
            case level of
                1 ->
                    h1 [ HA.class "mm-h1" ] [ renderClosedBlock mmInlineList ]

                2 ->
                    h2 [ HA.class "mm-h2" ] [ renderClosedBlock mmInlineList ]

                3 ->
                    h3 [ HA.class "mm-h3" ] [ renderClosedBlock mmInlineList ]

                4 ->
                    h4 [ HA.class "mm-h4" ] [ renderClosedBlock mmInlineList ]

                _ ->
                    h5 [ HA.class "mm-h5" ] [ renderClosedBlock mmInlineList ]

        ImageBlock label url ->
            img [ HA.src url, HA.class "mm-image" ] [ text label ]

        -- MMList blockList ->
        --     List.map (\b -> renderBlock ( b, mmState )) blockList
        --         |> div []
        MathDisplayBlock str ->
            displayMathText str

        -- div [] [ text <| "$$" ++ str ++ "$$" ]
        CodeBlock str ->
            pre [ HA.class "code" ]
                [ code [] [ text str ] ]

        ListItemBlock k mmInline ->
            let
                margin =
                    String.fromInt (18 * k)
                        ++ "px"
            in
            li
                [ HA.class "mm-ulist-item", style "margin-left" margin ]
                [ renderClosedBlock mmInline ]

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
            li
                [ style "margin-left" margin
                , HA.class "mm-olist-item"
                ]
                [ renderClosedBlock content ]

        QuotationBlock mmInline ->
            div
                [ HA.class "mm-quotation" ]
                [ renderClosedBlock mmInline ]

        PoetryBlock mmInline ->
            div
                [ HA.class "mm-poetry" ]
                [ renderClosedBlock mmInline ]

        HorizontalRuleBlock ->
            hr [] []

        ClosedBlock mmInline ->
            div [ HA.class "mm-closed-block" ] [ renderClosedBlock mmInline ]

        ErrorBlock errorMessage ->
            div [ HA.class "mm-error-message" ] [ text errorMessage ]


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
            span [ HA.class "mm-code" ]
                [ code [] [ text str ]
                ]

        InlineMath str ->
            inlineMathText str

        MMInlineList list ->
            div [ HA.class "mm-inlinelist" ] (List.map renderClosedBlock list)

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
