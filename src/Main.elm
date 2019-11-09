module Main exposing (..)

import Browser
import Html exposing (..)



--MAIN


main =
    Browser.document
        { init = init
        , update = update
        , view = \model -> { title = "Star • Chores", body = [ view model ] }
        , subscriptions = \_ -> Sub.none
        }



--INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( 0, Cmd.none )



--UPDATE


type alias Model =
    Int


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



--VIEW


view : Model -> Html Msg
view model =
    h1 [] [ text "Hello World" ]
