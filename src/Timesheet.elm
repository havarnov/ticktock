module Timesheet exposing (..)

import Html.App as App
import Html exposing (Html, text, div, button, table, tr, th)
import Html.Attributes exposing (disabled)
import Html.Events exposing (onClick)
import Task
import Time exposing (Time)
import Date exposing (Date)
import Time.DateTime as DateTime exposing (..)
import Json.Decode exposing (..)
import Http
import Entry


replace : List a -> a -> a -> List a
replace l existing new =
    List.map
        (\item ->
            if item == existing then
                new
            else
                item
        )
        l


type alias Model =
    { date : Date
    , current : Maybe Entry.Model
    , entries : List Entry.Model
    }


init : Date -> ( Model, Cmd Msg )
init date =
    update (TriggerUpdate date) { date = date, current = Nothing, entries = [] }


type Msg
    = Reset
    | Add Entry.Model
    | Stop Time
    | UpdateEntry Entry.Msg Entry.Model
    | FetchFail Http.Error
    | FetchSucceed Model
    | UpdateSucceed String
    | TriggerUpdate Date
    | TriggerPostUpdate
    | GetTimeAndThen (Time -> Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            ( model, Cmd.none )

        TriggerUpdate date ->
            ( model, getModel date )

        TriggerPostUpdate ->
            ( model, (postModel model) )

        Add entry ->
            case model.current of
                Just current ->
                    let
                        entries =
                            List.append model.entries [ { current | stop = Just entry.start } ]
                    in
                        update TriggerPostUpdate { model | entries = entries, current = Just entry }

                Nothing ->
                    update TriggerPostUpdate { model | current = Just entry }

        Stop t ->
            case model.current of
                Just current ->
                    let
                        entries =
                            List.append model.entries [ { current | stop = Just (DateTime.fromTimestamp t) } ]
                    in
                        update TriggerPostUpdate { model | entries = entries, current = Nothing }

                Nothing ->
                    ( model, Cmd.none )

        UpdateEntry entryMsg entry ->
            case entryMsg of
                Entry.Continue ->
                    update (clone entry) model

                _ ->
                    let
                        ( newEntry, newEntryMsg ) =
                            Entry.update entryMsg entry

                        updateEntryAndContinue =
                            ( { model | entries = (replace model.entries entry newEntry) }
                            , Cmd.map (\c -> UpdateEntry c newEntry) newEntryMsg
                            )
                    in
                        case model.current of
                            Just current ->
                                if current.start == entry.start then
                                    update TriggerPostUpdate { model | current = Just newEntry }
                                else
                                    updateEntryAndContinue

                            Nothing ->
                                updateEntryAndContinue

        FetchFail err ->
            Debug.log (toString err)
                ( model, Cmd.none )

        FetchSucceed newModel ->
            ( newModel, Cmd.none )

        UpdateSucceed message ->
            Debug.log message
                ( model, Cmd.none )

        GetTimeAndThen successHandler ->
            ( model, (Task.perform assertNeverHandler successHandler Time.now) )


startNew : Msg
startNew =
    GetTimeAndThen
        (\t ->
            Add
                { start = DateTime.fromTimestamp t
                , stop = Nothing
                , id = ""
                , desc = ""
                }
        )


clone : Entry.Model -> Msg
clone entry =
    GetTimeAndThen
        (\t ->
            Add
                { start = DateTime.fromTimestamp t
                , stop = Nothing
                , id = entry.id
                , desc = entry.desc
                }
        )


stop : Msg
stop =
    GetTimeAndThen Stop


view : Model -> Html Msg
view model =
    let
        dis =
            Maybe.withDefault True (Maybe.map (\c -> False) model.current)

        entries =
            case model.current of
                Just current ->
                    List.append model.entries [ current ]

                Nothing ->
                    model.entries
    in
        div
            []
            [ (viewEntries entries)
            , button [ onClick startNew ] [ text "Start" ]
            , button [ onClick stop, disabled dis ] [ text "Stop" ]
            , button [ onClick Reset ] [ text "Reset" ]
            ]


entryTableHead : Html Msg
entryTableHead =
    tr []
        [ th [] [ text "start" ]
        , th [] [ text "stop" ]
        , th [] [ text "id" ]
        , th [] [ text "desc" ]
        , th [] [ text "duration (ms)" ]
        , th [] [ text "-" ]
        ]


viewEntries : List Entry.Model -> Html Msg
viewEntries entries =
    table [] (entryTableHead :: (List.map viewEntry entries))


viewEntry : Entry.Model -> Html Msg
viewEntry entry =
    App.map (\a -> UpdateEntry a entry) (Entry.view entry)


assertNeverHandler : a -> b
assertNeverHandler =
    (\_ -> Debug.crash "This should never happen")


dateTimeDecoder : Decoder DateTime
dateTimeDecoder =
    customDecoder string fromISO8601


entryDecoder : Decoder Entry.Model
entryDecoder =
    object4 Entry.Model
        ("start" := dateTimeDecoder)
        ("stop" := (maybe dateTimeDecoder))
        ("id" := string)
        ("desc" := string)


entriesDecoder : Decoder (List Entry.Model)
entriesDecoder =
    list entryDecoder


dateDecoder: Decoder Date
dateDecoder =
    customDecoder string Date.fromString

modelDecoder : Decoder Model
modelDecoder =
    object3 Model
        ("date" := dateDecoder)
        ("current" := (maybe entryDecoder))
        ("entries" := entriesDecoder)


getModel : Date -> Cmd Msg
getModel date =
    let
        url =
            "/model.json"
    in
        Task.perform FetchFail FetchSucceed (Http.get modelDecoder url)


postModel : Model -> Cmd Msg
postModel model =
    let
        url =
            "/update"

        data =
            Http.string "{}"
    in
        Task.perform FetchFail UpdateSucceed (Http.post string url data)
