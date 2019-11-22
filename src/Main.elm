module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)



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
    ( { chores = [ "one", "two" ], newChore = "" }, Cmd.none )



--UPDATE


type alias Model =
    { chores : List String
    , newChore : String
    }


type Msg
    = NoOp
    | CompleteChore String
    | RemoveChore String
    | ChoreInput String
    | NewChore


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        CompleteChore chore ->
            let
                star c =
                    if c == chore then
                        c ++ "*"

                    else
                        c
            in
            ( { model | chores = List.map star model.chores }
            , Cmd.none
            )

        RemoveChore chore ->
            ( { model | chores = List.filter (\c -> c /= chore) model.chores }
            , Cmd.none
            )

        ChoreInput newChore ->
            ( { model | newChore = newChore }
            , Cmd.none
            )

        NewChore ->
            ( { model | chores = model.chores ++ [ model.newChore ], newChore = "" }
            , Cmd.none
            )



--VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Hello World" ]
        , ul [] <|
            List.map viewChore model.chores
        , input [ onInput ChoreInput, value model.newChore ] []
        , button [ onClick NewChore ] [ text "Add Chore" ]
        ]


viewChore : String -> Html Msg
viewChore chore =
    li [ onClick (CompleteChore chore) ]
        [ button [ onClick (RemoveChore chore) ]
            [ text "x" ]
        , text (" " ++ chore)
        ]
