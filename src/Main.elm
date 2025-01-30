module Main exposing (main)

import Browser
import Game exposing (nextGeneration)
import Grid exposing (Grid, initGrid, toggleCell, viewGrid)
import Html exposing (Html, button, div, table, td, text, tr)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Time


type alias Model =
    { grid : Grid, running : Bool }


initModel : Model
initModel =
    { grid = initGrid 10 10, running = False }


type Msg
    = ToggleCell Int Int
    | StartPause
    | Reset
    | Step


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleCell x y ->
            ( { model | grid = toggleCell x y model.grid }, Cmd.none )

        StartPause ->
            ( { model | running = not model.running }, Cmd.none )

        Reset ->
            ( { model | grid = initGrid 10 10, running = False }, Cmd.none )

        Step ->
            let
                _ =
                    Debug.log "Step pressed" model
            in
            ( { model | grid = nextGeneration model.grid }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ viewGrid model.grid ToggleCell
        , div []
            [ button [ onClick StartPause ]
                [ text
                    (if model.running then
                        "Pause"

                     else
                        "Start"
                    )
                ]
            , button [ onClick Step ] [ text "Step" ]
            , button [ onClick Reset ] [ text "Reset" ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.running then
        Time.every 500 (always Step)

    else
        Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
