module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task
import Http
import RemoteData
import Time exposing (Zone, Posix, Month(..), Weekday(..))
import Json.Decode as Decode

import SweepModels as SM
import DayModels as DM

type alias Model = 
  { zone: Zone
  , time: Posix
  , days: RemoteData.WebData (List DM.Day)
  , firstDay: Maybe String
  , selectedDay: Maybe String
  , showDayPicker: Bool
  , sweeps: RemoteData.WebData SM.Sweeps
  }

type Msg
  = SetTimeZone Zone
  | SetTime Posix
  | DaysResponse (RemoteData.WebData (List DM.Day))
  | SweepsResponse (RemoteData.WebData SM.Sweeps)
  | ShowDayPicker
  | SelectDay String
  | ChangeDay
  | CancelChangeDay

getDays : Cmd Msg
getDays =
  Http.get
    { url = "/api/days"
    , expect = Http.expectJson (RemoteData.fromResult >> DaysResponse) (Decode.list DM.dayDecoder)
    }

getTodaysSweeps : Cmd Msg
getTodaysSweeps =
  Http.get
    { url = "/api/sweeps/today"
    , expect = Http.expectJson (RemoteData.fromResult >> SweepsResponse) SM.sweepsDecoder
    }
    
getSweeps : Time.Posix -> Cmd Msg
getSweeps time =
  Http.get
    { url = "/api/sweeps/" ++ (Time.posixToMillis >> String.fromInt <| time)
    , expect = Http.expectJson (RemoteData.fromResult >> SweepsResponse) SM.sweepsDecoder
    }

init : () -> (Model, Cmd Msg)
init _ =
  ( { zone = Time.utc 
    , time = (Time.millisToPosix 0)
    , days = RemoteData.NotAsked
    , firstDay = Nothing
    , selectedDay = Nothing
    , showDayPicker = False
    , sweeps = RemoteData.NotAsked
    }
  , Cmd.batch 
    [ Task.perform SetTimeZone Time.here
    , getDays
    , getTodaysSweeps
    ]
  )

update msg model =
  case msg of
  SetTime newTime ->
    ( {model | time = newTime }
    , Cmd.none
    )
  SetTimeZone newZone ->
    ( { model | zone = newZone }
    , Task.perform SetTime Time.now
    )
  DaysResponse response ->
    let
      first =
        case response of
        RemoteData.Success days ->
          days
          |> List.head
          |> Maybe.map (\d ->
            d.id
            |> Time.posixToMillis
            |> String.fromInt
          )
        _ -> Nothing
        
    in
    ( { model | days = response, selectedDay = first, firstDay = first }
    , Cmd.none
    )
  SweepsResponse response ->
    ( { model | sweeps = response }
    , Cmd.none
    )
  ShowDayPicker ->
    ( { model | showDayPicker = True, selectedDay = model.firstDay }
    , Cmd.none
    )
  SelectDay day ->
    ( { model | selectedDay = Just day }
    , Cmd.none
    )
  CancelChangeDay ->
    ( { model | showDayPicker = False, selectedDay = Nothing }
    , Cmd.none
    )
  ChangeDay ->
    let
      newTime = 
        model.selectedDay
        |> Maybe.andThen (\d ->
          d
          |> String.toInt
          |> Maybe.map (\t ->
            Time.millisToPosix t
          )
        )
      sweepsCmd =
        newTime
        |> Maybe.map (\t -> getSweeps t)
        |> Maybe.withDefault Cmd.none
    in
    ( { model | showDayPicker = False, selectedDay = Nothing, time = Maybe.withDefault model.time newTime }
    , sweepsCmd
    )

toIntegerMonth month =
  case month of
    Jan -> "01"
    Feb -> "02"
    Mar -> "03"
    Apr -> "04"
    May -> "05"
    Jun -> "06"
    Jul -> "07"
    Aug -> "08"
    Sep -> "09"
    Oct -> "10"
    Nov -> "11"
    Dec -> "12"

toTextMonth month =
  case month of
    Jan -> "January"
    Feb -> "Febuary"
    Mar -> "March"
    Apr -> "April"
    May -> "May"
    Jun -> "June"
    Jul -> "July"
    Aug -> "August"
    Sep -> "September"
    Oct -> "October"
    Nov -> "November"
    Dec -> "December"

toWeekDay weekday =
  case weekday of
    Mon -> "Saturday"
    Tue -> "Tuesday"
    Wed -> "Wednesday"
    Thu -> "Thursday"
    Fri -> "Friday"
    Sat -> "Saturday"
    Sun -> "Sunday"

viewTime time zone =
  let
    weekDay =
      Time.toWeekday zone time
      |> toWeekDay
    day =
      Time.toDay zone time
      |> String.fromInt
      |> String.padLeft 2 '0'
    month = 
      Time.toMonth zone time 
      |> toTextMonth
    year = Time.toYear zone time
      |> String.fromInt
  in
  (weekDay ++ ", " ++ month ++ " " ++ day ++ " " ++ year)

viewActivity : SM.Activity -> Html Msg
viewActivity activity =
  case activity of
    SM.Maintenance ma ->
      tr [] [
        td [] [ text ma.address ],
        td [] [ text <| Maybe.withDefault "" <| Maybe.map SM.divisionToStr ma.division ],
        td [] [ text <| Maybe.withDefault "" <| Maybe.map SM.locationToStr ma.location ],
        td [] [ text "Illegal Dump" ],
        td [] [ ]
      ]
    SM.Division da ->
      tr [] [
        td [] [ text <| da.address ++ " with cross streets " ++ da.crossStreetOne ++ " and " ++ da.crossStreetTwo ],
        td [] [ text <| Maybe.withDefault "" <| Maybe.map SM.divisionToStr da.division ],
        td [] [ text <| Maybe.withDefault "" <| Maybe.map SM.locationToStr da.location ],
        td [] [ text "Sweep" ],
        td [] [ text da.comments ]
      ]

viewDatePicker model date =
  h1 [] (
    case model.showDayPicker of
    False ->
      [ span [] [ text "Sweeps for " ]
      , text <| viewTime date model.zone
      , (case model.days of
        RemoteData.Success _ -> button [ onClick ShowDayPicker ] [ text "Change Date" ]
        _ -> text ""
      )
      ]
    True ->
      [ select [ onInput SelectDay ] 
        (case model.days of
          RemoteData.Success days ->
            days
            |> List.map (\d -> option [ value <| String.fromInt <| Time.posixToMillis d.id ] [ text <| viewTime d.id model.zone ])
          _ -> []
        )
      , button [ onClick ChangeDay ] [ text "Accept" ]
      , button [ onClick CancelChangeDay ] [ text "Cancel" ]
      ]
  )

viewSweeps : RemoteData.WebData SM.Sweeps -> Html Msg
viewSweeps sweeps =
  case sweeps of
      RemoteData.NotAsked ->
        div [] [ text "Loading" ]
      RemoteData.Loading ->
        div [] [ text "Loading" ]
      RemoteData.Success sweep ->
          div [ ] 
            [ p [] [ a [ href sweep.url ] [ text <| "Download pdf: " ++ sweep.name ]]
            , table [] [
              thead [] [
                tr [] [
                  th [ class "w5" ] [ text "Address" ],
                  th [] [ text "Division" ],
                  th [] [ text "Location" ],
                  th [] [ text "Action Type" ],
                  th [] [ text "Comments" ]
                ]
              ],
              tbody []
                (sweep.activities
                |> List.map viewActivity)
              ]
            ]
      RemoteData.Failure error ->
        div [] [ text "No Sweeps loaded for today" ]

errorToString : Http.Error -> String
errorToString err =
    case err of
        Http.Timeout ->
            "Timeout exceeded"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus status ->
            "Bad Status: " ++ String.fromInt status

        Http.BadBody text ->
            "Unexpected response from api: " ++ text

        Http.BadUrl url ->
            "Malformed url: " ++ url

view model =
  div []
    [ viewDatePicker model model.time
    , viewSweeps model.sweeps
    ]

subscriptions model = 
  Sub.none

main =
    Browser.element 
      { init = init
      , view = view
      , update = update
      , subscriptions = subscriptions
      }