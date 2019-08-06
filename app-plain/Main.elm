module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes as HA exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Keyed as Keyed
import Json.Encode
import MMDiffer exposing (EditRecord)
import MMarkdown
import Random
import Strings
import Style exposing (..)
import Task


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
    , editRecord : EditRecord (Html Msg)
    , counter : Int
    , seed : Int
    }


type Msg
    = Clear
    | GetContent String
    | GenerateSeed
    | NewSeed Int
    | RestoreText
    | RefreshText
    | ExampleText


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { sourceText = Strings.initialText
            , counter = 0
            , seed = 0
            , editRecord = MMDiffer.createRecord (MMarkdown.toHtml []) Strings.initialText
            }
    in
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetContent str ->
            ( { model
                | sourceText = str
                , editRecord = MMDiffer.update (model.counter + 1) (MMarkdown.toHtml []) model.editRecord str
                , counter = model.counter + 1
              }
            , Cmd.none
            )

        GenerateSeed ->
            ( model, Random.generate NewSeed (Random.int 1 10000) )

        NewSeed newSeed ->
            ( { model | seed = newSeed }, Cmd.none )

        Clear ->
            ( { model
                | sourceText = ""
                , editRecord = MMDiffer.createRecord (MMarkdown.toHtml []) ""
                , counter = model.counter + 1
              }
            , Cmd.none
            )

        RestoreText ->
            ( { model
                | counter = model.counter + 1
                , sourceText = Strings.initialText
                , editRecord = MMDiffer.createRecord (MMarkdown.toHtml []) Strings.initialText
              }
            , Cmd.none
            )

        RefreshText ->
            ( { model
                | counter = model.counter + 1
                , editRecord = MMDiffer.createRecord (MMarkdown.toHtml []) model.sourceText
              }
            , Cmd.none
            )

        ExampleText ->
            ( { model
                | counter = model.counter + 1
                , sourceText = Strings.mathExampleText
              }
            , Cmd.none
            )



--
-- VIEW FUNCTIONS
---


view : Model -> Html Msg
view model =
    div outerStyle
        [ display model
        ]


display : Model -> Html Msg
display model =
    div []
        [ h1 [ style "margin-left" "20px" ] [ text "MMarkdown Demo (Plain)" ]
        , p [ style "margin-left" "20px", style "margin-bottom" "0", style "margin-top" "0" ] [ text "This project is in its very earliest stages ..." ]
        , p [ style "margin-left" "20px", style "margin-top" "0" ] [ text "Edit or write new MMarkdown below." ]
        , editor model
        , renderedSource model
        , p [ style "clear" "left", style "margin-left" "20px", style "margin-top" "-20px" ] [ clearButton 60, restoreTextButton 80, refreshButton 80 ]
        ]


label text_ =
    p labelStyle [ text text_ ]


editor : Model -> Html Msg
editor model =
    textarea (editorTextStyle ++ [ onInput GetContent, value model.sourceText ]) []


renderedSource : Model -> Html Msg
renderedSource model =
    let
        token =
            String.fromInt model.counter
    in
    Keyed.node "div"
        renderedSourceStyle
        (List.map2 (\x y -> ( x, y )) model.editRecord.idList model.editRecord.renderedParagraphs)



-- [ ( String.fromInt model.counter, MMarkdown.toHtml [] (model.sourceText ++ "\n\n") ) ]
--
-- BUTTONS
--


clearButton width =
    button ([ onClick Clear ] ++ buttonStyle colorBlue width) [ text "Clear" ]


restoreTextButton width =
    button ([ onClick RestoreText ] ++ buttonStyle colorBlue width) [ text "Restore" ]


refreshButton width =
    button ([ onClick RefreshText ] ++ buttonStyle colorBlue width) [ text "Refresh" ]


exampleButton width =
    button ([ onClick ExampleText ] ++ buttonStyle colorBlue width) [ text "Example 2" ]
