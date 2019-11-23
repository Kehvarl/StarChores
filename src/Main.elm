module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Svg exposing (polygon)
import Svg.Attributes as SA exposing (height, points, stroke, viewBox, width)



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
    ( { chores = [ Chore "one" 0 ], newChore = "" }
    , Cmd.none
    )



--UPDATE


type alias Chore =
    { name : String
    , stars : Int
    }


type alias Model =
    { chores : List Chore
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
                    if c.name == chore then
                        { c | stars = c.stars + 1 }

                    else
                        c
            in
            ( { model | chores = List.map star model.chores }
            , Cmd.none
            )

        RemoveChore chore ->
            ( { model | chores = List.filter (\c -> c.name /= chore) model.chores }
            , Cmd.none
            )

        ChoreInput newChore ->
            ( { model | newChore = newChore }
            , Cmd.none
            )

        NewChore ->
            ( { model | chores = model.chores ++ [ Chore model.newChore 0 ], newChore = "" }
            , Cmd.none
            )



--VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "My Chores Chart" ]
        , ul [] <|
            List.map viewChore model.chores
        , input [ onInput ChoreInput, value model.newChore ] []
        , button [ onClick NewChore ] [ text "Add Chore" ]
        ]


viewChore : Chore -> Html Msg
viewChore chore =
    li [ onClick (CompleteChore chore.name) ]
        [ button [ onClick (RemoveChore chore.name) ]
            [ text "x" ]
        , text (" " ++ chore.name)
        , span [] <| List.repeat chore.stars viewStar
        ]


viewStar : Html Msg
viewStar =
    Svg.svg
        [ SA.width "25"
        , SA.height "20"
        , SA.viewBox "0 0 25 17"
        ]
        [ polygon
            [ SA.fill "gold"
            , SA.stroke "yellow"
            , SA.points "10,1 4,20 19,8 1,8 16,20"
            ]
            []
        ]
