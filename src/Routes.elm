module Routes exposing (..)

import String
import Date
import Navigation exposing (Location)
import UrlParser exposing (Parser, custom, parse, (</>), format, int, oneOf, s, string)


type Route
    = Timesheets
    | Timesheet Date.Date


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ format Timesheets (s "timesheets")
        , format Timesheet (s "timesheet" </> date)
        ]

date : Parser (Date.Date -> a) a
date =
    custom "DATE" Date.fromString


decode : Location -> Result String Route
decode location =
    parse identity routeParser (String.dropLeft 1 location.hash)
