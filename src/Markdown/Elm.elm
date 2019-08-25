module Markdown.Elm exposing (toHtml)

import Block exposing (BlockContent(..), MMBlock(..))
import Html exposing (Html)
import Html.Attributes as HA exposing (style)
import Json.Encode
import LineType exposing (BalancedType(..), BlockType(..), MarkdownType(..))
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

        MMBlock (MarkdownBlock (Heading k)) level blockContent ->
            renderHeading k blockContent

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

        Line arg ->
            Html.span [] (List.map renderToHtmlMsg arg)

        Paragraph arg ->
            Html.p [] (List.map renderToHtmlMsg arg)

        Error arg ->
            Html.p [] (List.map renderToHtmlMsg arg)


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
