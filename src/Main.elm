module Main exposing (..)

import Html exposing (Html, text)
import Html.App as App
import Navigation
import Routes
import Timesheet
import Timesheets


-- model


type ProgramState
    = Uninitialized
    | Timesheet Timesheet.Model
    | Timesheets Timesheets.Model


type alias Model =
    ProgramState


type Msg
    = TimesheetMsg Timesheet.Msg
    | TimesheetsMsg Timesheets.Msg


init : Result String Routes.Route -> ( Model, Cmd Msg )
init result =
    Debug.log (toString result)
        urlUpdate
        result
        Uninitialized



--update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimesheetMsg timesheetMsg ->
            case model of
                Timesheet entries ->
                    let
                        ( newEntries, newTimesheetMsg ) =
                            Timesheet.update timesheetMsg entries
                    in
                        ( (Timesheet newEntries)
                        , Cmd.map (\c -> TimesheetMsg c) newTimesheetMsg
                        )

                _ ->
                    ( model, Cmd.none )

        TimesheetsMsg timesheetsMsg ->
            case model of
                Timesheets listOfTimesheets ->
                    let
                        ( newTimesheets, newTimesheetsMsg ) =
                            Timesheets.update timesheetsMsg listOfTimesheets
                    in
                        ( (Timesheets newTimesheets)
                        , Cmd.map (\c -> TimesheetsMsg c) newTimesheetsMsg
                        )

                _ ->
                    ( model, Cmd.none )


urlUpdate : Result String Routes.Route -> Model -> ( Model, Cmd Msg )
urlUpdate result model =
    case result of
        Err errMsg ->
            ( model, Navigation.modifyUrl "#timesheets" )

        Ok (Routes.Timesheets) ->
            let
                (listOfTimesheets, cmdmsg) = Timesheets.init

                msg =
                    Cmd.map (\c -> TimesheetsMsg c) cmdmsg
            in
                ( Timesheets listOfTimesheets, msg)

        Ok (Routes.Timesheet date) ->
            let
                ( entries, cmdmsg ) =
                    Timesheet.init date

                msg =
                    Cmd.map (\c -> TimesheetMsg c) cmdmsg
            in
                ( (Timesheet entries), msg )



-- view


view : Model -> Html Msg
view model =
    case model of
        Timesheets listOfTimesheets ->
            App.map (\a -> TimesheetsMsg a) (Timesheets.view listOfTimesheets)

        Timesheet entries ->
            App.map (\a -> TimesheetMsg a) (Timesheet.view entries)

        Uninitialized ->
            text "loading..."



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- main


main : Program Never
main =
    Navigation.program (Navigation.makeParser Routes.decode)
        { init = init
        , view = view
        , update = update
        , urlUpdate = urlUpdate
        , subscriptions = subscriptions
        }
