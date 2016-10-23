module Entry exposing (..)

import Html exposing (Html, tr, td, text, input, button)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput, onClick)
import Time.DateTime as DateTime exposing (..)

type alias Model = {
    start: DateTime,
    stop: Maybe DateTime,
    id: String,
    desc: String
}

type Msg =
    SetId String |
    SetDesc String |
    Continue

calcDuration : Model -> Maybe Int
calcDuration entry =
    Maybe.map2
        (\start stop ->
            let
                d = delta stop start
            in 
                d.milliseconds)
        (Just entry.start)
        entry.stop

update : Msg -> Model -> (Model, Cmd Msg)
update msg entry =
    case msg of
        SetId id -> ({entry | id = id}, Cmd.none)
        SetDesc desc ->
            ({entry | desc = desc}, Cmd.none)
        Continue ->
            (entry, Cmd.none)

view : Model -> Html Msg
view entry =
    tr [] [
        td [] [text (DateTime.toISO8601 entry.start)],
        td [] [text (Maybe.withDefault "--" (Maybe.andThen entry.stop (\s -> Just (DateTime.toISO8601 s))))],
        td [] [
            input [onInput SetId, value entry.id] []
        ],
        td [] [input [onInput SetDesc, value entry.desc] []],
        td [] [text (Maybe.withDefault "" (Maybe.map toString (calcDuration entry)))],
        td [] [button [onClick Continue] [text "continue"]]
    ]