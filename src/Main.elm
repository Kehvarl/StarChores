module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Svg exposing (polygon)
import Svg.Attributes as SA exposing (height, points, stroke, viewBox, width)
import Task
import Time



--MAIN


main =
    Browser.document
        { init = init
        , update = update
        , view = \model -> { title = "Star • Chores", body = [ view model ] }
        , subscriptions = subscriptions
        }



--INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { chores = [ Chore "Drink Water!" [] Glass ]
      , newChore = ""
      , curTime = Time.millisToPosix 0
      }
    , Cmd.none
    )



--UPDATE


type alias Completion =
    { time : Time.Posix }


type ChoreIcon
    = Star
    | Glass


type alias Chore =
    { name : String
    , completed : List Completion
    , icon : ChoreIcon
    }


type alias Model =
    { chores : List Chore
    , newChore : String
    , curTime : Time.Posix
    }


type Msg
    = NoOp
    | CompleteChore String
    | RemoveChore String
    | ChoreInput String
    | NewChore
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        CompleteChore chore ->
            let
                complete c =
                    if c.name == chore then
                        { c | completed = c.completed ++ [ Completion model.curTime ] }

                    else
                        c
            in
            ( { model | chores = List.map complete model.chores }
            , Task.perform Tick Time.now
            )

        RemoveChore chore ->
            ( { model | chores = List.filter (\c -> c.name /= chore) model.chores }
            , Task.perform Tick Time.now
            )

        ChoreInput newChore ->
            ( { model | newChore = newChore }
            , Task.perform Tick Time.now
            )

        NewChore ->
            let
                exists =
                    List.filter (\c -> c.name == model.newChore) model.chores
            in
            if exists == [] then
                ( { model | chores = model.chores ++ [ Chore model.newChore [] Star ], newChore = "" }
                , Task.perform Tick Time.now
                )

            else
                ( { model
                    | chores =
                        List.map
                            (\c ->
                                if c.name == model.newChore then
                                    Chore c.name (c.completed ++ [ Completion model.curTime ]) Star

                                else
                                    c
                            )
                            model.chores
                    , newChore = ""
                  }
                , Task.perform Tick Time.now
                )

        Tick newTime ->
            ( { model | curTime = newTime }
            , Cmd.none
            )



--SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



--VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "My Chores Chart" ]
        , h3 [] [ text "Give yourself some credit" ]
        , ul [] <|
            List.map viewChore model.chores
        , input [ onInput ChoreInput, value model.newChore ] []
        , button [ onClick NewChore ] [ text "Add Task" ]
        ]


viewChore : Chore -> Html Msg
viewChore chore =
    li [ onClick (CompleteChore chore.name) ]
        [ button
            [ onClick (RemoveChore chore.name) ]
            [ text "x" ]
        , text (" " ++ chore.name)
        , span [] <| List.map (viewComplete chore.icon) chore.completed
        ]


viewComplete : ChoreIcon -> Completion -> Html Msg
viewComplete choreIcon completion =
    case choreIcon of
        Star ->
            viewStar completion

        Glass ->
            viewGlass completion


viewStar : Completion -> Html Msg
viewStar star =
    Svg.svg
        [ SA.width "25"
        , SA.height "20"
        , SA.viewBox "0 0 25 20"
        ]
        --Draw a star
        [ polygon
            [ SA.fill "gold"
            , SA.stroke "yellow"
            , SA.points "10,1 4,20 19,8 1,8 16,20"
            ]
            [ Svg.title
                []
                [ text (stringFromPosix star.time) ]
            ]
        ]


viewGlass : Completion -> Html Msg
viewGlass star =
    Svg.svg
        [ SA.width "25"
        , SA.height "20"
        , SA.viewBox "0 0 25 20"
        ]
        --Draw a star
        [ polygon
            [ SA.fill "White"
            , SA.stroke "black"
            , SA.points "3,3 17,3 15,18 8,18"
            ]
            [ Svg.title
                []
                [ text (stringFromPosix star.time) ]
            ]
        , polygon
            [ SA.fill "blue"
            , SA.stroke "Blue"
            , SA.points "6,8 14,8 15,18 8,18"
            ]
            [ Svg.title
                []
                [ text (stringFromPosix star.time) ]
            ]
        ]


stringFromPosix : Time.Posix -> String
stringFromPosix posix =
    let
        year =
            String.fromInt (Time.toYear Time.utc posix)

        month =
            stringFromMonth (Time.toMonth Time.utc posix)

        day =
            String.fromInt (Time.toDay Time.utc posix)

        hour =
            String.fromInt (Time.toHour Time.utc posix)

        minute =
            String.fromInt (Time.toMinute Time.utc posix)

        second =
            String.fromInt (Time.toSecond Time.utc posix)
    in
    year ++ "/" ++ month ++ "/" ++ day ++ " -- " ++ hour ++ ":" ++ minute ++ ":" ++ second


stringFromMonth : Time.Month -> String
stringFromMonth monthTime =
    case monthTime of
        Time.Jan ->
            "Jan"

        Time.Feb ->
            "Feb"

        Time.Mar ->
            "Mar"

        Time.Apr ->
            "Apr"

        Time.May ->
            "May"

        Time.Jun ->
            "Jun"

        Time.Jul ->
            "Jul"

        Time.Aug ->
            "Aug"

        Time.Sep ->
            "Sep"

        Time.Oct ->
            "Oct"

        Time.Nov ->
            "Nov"

        Time.Dec ->
            "Dec"
