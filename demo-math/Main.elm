module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes as HA exposing (..)
import MMarkdown
import Strings


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , update = update
        , init = init
        , subscriptions = subscriptions
        }


type alias Model =
    { sourceText : String
    , rhPanelText : String
    }


type Msg
    = NoOp


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { sourceText = Strings.initialText
            , rhPanelText = Strings.notes
            }
    in
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div outerStyle
        [ h1 []
            [ text "MMarkdown Demo" ]
        , MMarkdown.toHtml
            displayStyle
            model.sourceText
        ]



-- STYLE --


outerStyle =
    [ HA.style "padding" "40px"
    , HA.style "background-color" "#ccc"
    , HA.style "width" "900px"
    ]


displayStyle =
    [ HA.style "width" "400px"
    , HA.style "height" "500px"
    , HA.style "padding" "20px"
    , HA.style "overflow" "scroll"
    , HA.style "background-color" "#eee"
    ]
