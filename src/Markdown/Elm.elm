module Markdown.Elm exposing (intercalate, toHtml)

import Block exposing (BlockContent(..), MMBlock(..))
import Html exposing (Html)
import Html.Attributes as HA exposing (style)
import Json.Encode
import LineType exposing (BalancedType(..), BlockType(..), MarkdownType(..))
import List.Extra
import MMInline exposing (MMInline(..))
import Tree exposing (Tree)


toHtml : Tree MMBlock -> Html msg
toHtml tree =
    Tree.foldl (\block elements -> renderBlock block :: elements) [] tree
        |> (\x -> Html.div [] (List.reverse x))


renderBlock : MMBlock -> Html msg
renderBlock block =
    case block of
        MMBlock (MarkdownBlock Plain) level blockContent ->
            renderBlockContent blockContent

        MMBlock (MarkdownBlock Blank) level blockContent ->
            renderBlockContent blockContent

        MMBlock (MarkdownBlock (Heading k)) level blockContent ->
            renderHeading k blockContent

        MMBlock (MarkdownBlock Quotation) level blockContent ->
            renderQuotation blockContent

        MMBlock (MarkdownBlock Poetry) level blockContent ->
            renderPoetry blockContent

        MMBlock (MarkdownBlock UListItem) level blockContent ->
            renderUListItem level blockContent

        MMBlock (MarkdownBlock HorizontalRule) level blockContent ->
            Html.hr [ HA.class "mm-thematic-break" ] []

        MMBlock (MarkdownBlock LineType.Image) level blockContent ->
            let
                _ =
                    Debug.log "IMG (BC)" blockContent
            in
            renderBlockContent blockContent

        MMBlock (BalancedBlock DisplayMath) level blockContent ->
            case blockContent of
                T str ->
                    displayMathText str

                _ ->
                    displayMathText ""

        MMBlock (BalancedBlock Verbatim) level blockContent ->
            case blockContent of
                T str ->
                    Html.pre [] [ Html.text str ]

                _ ->
                    displayMathText ""

        MMBlock (BalancedBlock DisplayCode) level blockContent ->
            case blockContent of
                T str ->
                    Html.pre [] [ Html.text str ]

                _ ->
                    displayMathText ""

        _ ->
            Html.div [] [ Html.text "Not implemented" ]


renderUListItem : Int -> BlockContent -> Html msg
renderUListItem k blockContent =
    let
        margin =
            String.fromInt (18 * k)
                ++ "px"

        label =
            case k of
                1 ->
                    "• "

                2 ->
                    "◊ "

                3 ->
                    "† "

                4 ->
                    "‡ "

                _ ->
                    "N. "
    in
    Html.li
        [ style "margin-left" margin
        , HA.class "mm-olist-item"
        ]
        [ renderBlockContent blockContent ]



-- renderOListItem : Int -> BlockContent -> Html msg
-- renderOListItem k blockContent =
--     let
--         margin =
--             String.fromInt (18 * k)
--                 ++ "px"
--
--         label =
--             case k of
--                 1 ->
--                     String.fromInt mmState.itemIndex1 ++ ". "
--
--                 2 ->
--                     alphabet mmState.itemIndex2 ++ ". "
--
--                 3 ->
--                     romanNumeral mmState.itemIndex2 ++ ". "
--
--                 4 ->
--                     String.fromInt mmState.itemIndex4 ++ ". "
--
--                 _ ->
--                     "N. "
--     in
--     li
--         [ style "margin-left" margin
--         , HA.class "mm-olist-item"
--         ]
--         [ renderBlockContent blockContent ]


renderHeading : Int -> BlockContent -> Html msg
renderHeading k blockContent =
    case k of
        1 ->
            Html.h1 [] [ renderBlockContent blockContent ]

        2 ->
            Html.h2 [] [ renderBlockContent blockContent ]

        3 ->
            Html.h3 [] [ renderBlockContent blockContent ]

        4 ->
            Html.h4 [] [ renderBlockContent blockContent ]

        _ ->
            Html.h5 [] [ renderBlockContent blockContent ]


renderQuotation : BlockContent -> Html msg
renderQuotation blockContent =
    Html.div
        [ HA.class "mm-quotation" ]
        [ renderBlockContent blockContent ]


renderPoetry : BlockContent -> Html msg
renderPoetry blockContent =
    Html.div
        [ HA.class "mm-poetry" ]
        [ renderBlockContent blockContent ]


renderBlockContent : BlockContent -> Html msg
renderBlockContent blockContent =
    case blockContent of
        M mmInline ->
            renderToHtmlMsg mmInline

        T str ->
            Html.div [] [ Html.text str ]


renderToHtmlMsg : MMInline -> Html msg
renderToHtmlMsg mmInline =
    case mmInline of
        OrdinaryText str ->
            Html.span [] [ Html.text str ]

        ItalicText str ->
            Html.em [] [ Html.text str ]

        BoldText str ->
            Html.strong [] [ Html.text str ]

        Code str ->
            Html.code [] [ Html.text str ]

        InlineMath str ->
            inlineMathText str

        StrikeThroughText str ->
            strikethrough str

        BracketedText str ->
            Html.span [] [ Html.text <| "[" ++ str ++ "]" ]

        Link url label ->
            Html.a [ HA.href url ] [ Html.text label ]

        MMInline.Image label url ->
            Html.img [ HA.src url, HA.class "mm-image" ] [ Html.text label ]

        Line arg ->
            let
                renderedLines =
                    List.map renderToHtmlMsg arg
            in
            Html.span [] (List.map (\rl -> Html.span [ style "margin-right" "5px" ] [ rl ]) renderedLines)

        Paragraph arg ->
            Html.p [] (List.map renderToHtmlMsg arg)

        Error arg ->
            Html.p [] (List.map renderToHtmlMsg arg)


intercalate : a -> List a -> List a
intercalate x list =
    List.Extra.intercalate [ x ] (List.map (\item -> [ item ]) list)


strikethrough : String -> Html msg
strikethrough str =
    Html.span [ HA.class "strikethrough" ] [ Html.text str ]



-- MATH --


mathText : String -> Html msg
mathText content =
    Html.node "math-text"
        [ HA.class "mm-display-math", HA.property "content" (Json.Encode.string content) ]
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
