module Timesheets exposing (..)

import Html exposing (Html, text, div, ul, li, a, input, Attribute)
import Html.Attributes exposing (href, type')
import Html.Events exposing (onInput, on)
import Date exposing (Date)
import Task
import Navigation
import Http
import Json.Decode exposing (..)


type alias Model =
    { listOfTimesheets: List Date
    , today: Date
    }


type Msg
    = Today String
    | Update
    | UpdateSuccess Model
    | HttpFailure Http.Error


init : ( Model, Cmd Msg )
init =
    update Update {today = Date.fromTime 0, listOfTimesheets = []}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Update ->
            ( model, getTimesheets )

        UpdateSuccess newModel ->
            ( newModel, Cmd.none )

        Today dateString ->
            let
                dateResult = Date.fromString dateString
            in
                case dateResult of
                    Ok date ->
                        model ! [ Navigation.newUrl ("#timesheet/" ++ dateString) ]
                    Err _ ->
                        ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


formatMonth : Date.Month -> String
formatMonth month =
    case month of
        Date.Jan ->
            "01"

        Date.Feb ->
            "02"

        Date.Mar ->
            "03"

        Date.Apr ->
            "04"

        Date.May ->
            "05"

        Date.Jun ->
            "06"

        Date.Jul ->
            "07"

        Date.Aug ->
            "08"

        Date.Sep ->
            "09"

        Date.Oct ->
            "10"

        Date.Nov ->
            "11"

        Date.Dec ->
            "12"


format : Date -> String
format date =
    (toString (Date.year date))
        ++ "-"
        ++ formatMonth (Date.month date)
        ++ "-"
        ++ (toString (Date.day date))


viewDate : Date -> Html Msg
viewDate date =
    let
        formattedDate =
            format date
    in
        li [] [ a [ href ("#timesheet/" ++ formattedDate) ] [ text formattedDate ] ]


view : Model -> Html Msg
view model =
    let

        existing =
            List.map viewDate (model.today :: model.listOfTimesheets)
    in
        div [] [
            (ul [] existing),
            input [type' "date", onInput Today] []
        ]

getTimesheets : Cmd Msg
getTimesheets =
    let
        url =
            "/timesheets.json"
    in
        Task.perform HttpFailure UpdateSuccess (Http.get modelDecoder url)


dateDecoder : Decoder Date
dateDecoder =
    customDecoder string Date.fromString

listOfTimesheetsDecoder : Decoder (List Date)
listOfTimesheetsDecoder =
    list dateDecoder

modelDecoder : Decoder Model
modelDecoder =
    object2 Model
        ("listOfTimesheets" := listOfTimesheetsDecoder)
        ("today" := dateDecoder)

