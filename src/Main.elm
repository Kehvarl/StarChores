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
    | CompleteChore String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        CompleteChore chore ->
            Debug.todo ("handle CompleteChore " ++ chore)



--VIEW


view : Model -> Html Msg
view model =
    h1 [] [ text "Hello World" ]
