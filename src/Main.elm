module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array
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
  , showUnconfirmed: Bool
  }

type Msg
  = SetTimeZone Zone
  | SetTime Posix
  | DaysResponse (RemoteData.WebData (List DM.Day))
  | SweepsResponse (RemoteData.WebData SM.Sweeps)
  | ShowDayPicker
  | SelectDay String
  | GoToPreviousDay
  | GoToNextDay
  | ChangeDay
  | CancelChangeDay
  | SetShowUnconfirmed Bool

getDays : Cmd Msg
getDays =
  Http.get
    { url = "/api/days"
    , expect = Http.expectJson (RemoteData.fromResult >> DaysResponse) (Decode.list DM.dayDecoder)
    }
    
getSweeps : Time.Posix -> Time.Zone -> Cmd Msg
getSweeps time zone =
  Http.get
    { url = "/api/sweeps/" ++ (getDate time zone)
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
    , showUnconfirmed = True
    }
  , Cmd.batch 
    [ Task.perform SetTimeZone Time.here
    , getDays
    ]
  )

posixToString posix =
  posix
  |> Time.posixToMillis
  |> String.fromInt

millisecondsInADay = 86400000

getSweepsForDay zone day =
  let
      newTime = 
        day
        |> Maybe.andThen (\d ->
          d
          |> String.toInt
          |> Maybe.map (\t ->
            Time.millisToPosix t
          )
        )
      sweepsCmd =
        newTime
        |> Maybe.map (\t -> getSweeps t zone)
        |> Maybe.withDefault Cmd.none
    in
    (newTime, sweepsCmd)

update msg model =
  case msg of
  SetTime newTime ->
    ( {model | time = newTime }
    , getSweeps newTime model.zone
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
            |> posixToString
          )
        _ -> Nothing
    in
    ( { model | days = response, selectedDay = first, firstDay = first }
    , Cmd.none
    )
  SweepsResponse response ->
    let
      newTime =
        case response of
        RemoteData.Success sweeps ->
          Just sweeps.date
        _ -> Nothing
    in
    ( { model | sweeps = response, time = Maybe.withDefault model.time newTime }
    , Cmd.none
    )
  ShowDayPicker ->
    let
      currentlySelectedDay = 
        case model.sweeps of
        RemoteData.Success sweep ->
          case sweep.activities of
            [] -> model.firstDay
            _ ->
              sweep.date
              |> posixToString
              |> Just
        _ ->
          model.firstDay
    in
    ( { model | showDayPicker = True, selectedDay = currentlySelectedDay }
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
      (newTime, sweepsCmd) = 
        getSweepsForDay model.zone model.selectedDay
    in
    ( { model | showDayPicker = False, selectedDay = Nothing, time = Maybe.withDefault model.time newTime }
    , sweepsCmd
    )
  GoToPreviousDay ->
    let
      time =
        model.time
        |> Time.posixToMillis
        |> (\t -> t - millisecondsInADay)
        |> Time.millisToPosix
    in
    ( model
    , getSweeps time model.zone )
  GoToNextDay ->
    let
      time =
        model.time
        |> Time.posixToMillis
        |> (+) millisecondsInADay
        |> Time.millisToPosix
    in
    ( model
    , getSweeps time model.zone )
  SetShowUnconfirmed value ->
    ( { model | showUnconfirmed = value }
    , Cmd.none
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
    Mon -> "Monday"
    Tue -> "Tuesday"
    Wed -> "Wednesday"
    Thu -> "Thursday"
    Fri -> "Friday"
    Sat -> "Saturday"
    Sun -> "Sunday"

getDate time zone =
  let
    day =
      Time.toDay zone time
      |> String.fromInt
      |> String.padLeft 2 '0'
    month =
      Time.toMonth zone time
      |> toIntegerMonth
    year =
      Time.toYear zone time
      |> String.fromInt
  in
  year ++ "-" ++ month ++ "-" ++ day

viewTime time zone =
  let
    day =
      Time.toDay zone time
      |> String.fromInt
      |> String.padLeft 2 '0'
    weekDay =
      Time.toWeekday zone time
      |> toWeekDay
    month =
      Time.toMonth zone time
      |> toIntegerMonth
    year =
      Time.toYear zone time
      |> String.fromInt
  in
  month ++ "/" ++ day ++ "/" ++ year ++ ", " ++ weekDay

viewHeaderTime time zone =
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
      tr [] 
        [ td [] [ text ma.address ]
        , td [] [ text <| Maybe.withDefault "" <| Maybe.map SM.divisionToStr ma.division ]
        , td [] [ text <| Maybe.withDefault "" <| Maybe.map SM.locationToStr ma.location ]
        , td [] [ text "Illegal Dump" ]
        , td [] [ text ma.comments ]
        , td [] [ text <| SM.maintenanceStatusToStr ma.status ]
        ]
    SM.Division da ->
      tr []
        [ td [] 
            [ div [] [ text <| da.address ]
            , div [] [ text "with cross streets:" ]
            , div [] [ text da.crossStreetOne ]
            , div [] [ text da.crossStreetTwo ]
            ]
        , td [] [ text <| Maybe.withDefault "" <| Maybe.map SM.divisionToStr da.division ]
        , td [] [ text <| Maybe.withDefault "" <| Maybe.map SM.locationToStr da.location ]
        , td [] [ text "Sweep" ]
        , td [] [ text da.comments ]
        , td [] [ text <| SM.statusToStr da.status ]
        ]
    SM.Future fa ->
      tr [ ]
        [ td [] 
            [ div [] 
              [ div [ class "unconfirmed" ] [ text "Unconfirmed" ]
              , text <| fa.address
              ]
            , div [] [ text "with cross streets:" ]
            , div [] [ text fa.crossStreetOne ]
            , div [] [ text fa.crossStreetTwo ]
            ]
        , td [] [ text <| Maybe.withDefault "" <| Maybe.map SM.divisionToStr fa.division ]
        , td [] [ text <| Maybe.withDefault "" <| Maybe.map SM.locationToStr fa.location ]
        , td [ class "unconfirmed" ] [ text "Potential Future Sweep" ]
        , td [] [ text fa.comments ]
        , td [] [ text <| SM.statusToStr fa.status ]
        ]
viewDayButton label msg =
  button [ onClick msg, class "btn" ] [ text label ]

viewDatePicker model =
    case model.showDayPicker of
    False ->
        h1 [ class "flex justify-center flex-column" ] 
          [ div [ class "tc" ] [ text <| "Sweeps for " ++ (viewHeaderTime model.time model.zone) ]
          , div [ class "flex justify-center" ] 
            [ viewDayButton "⇐ Previous Day" GoToPreviousDay
            , (case model.days of
              RemoteData.Success _ -> button [ onClick ShowDayPicker, class "btn" ] [ text "📅 Change Date" ]
              _ -> text ""
              )
            , viewDayButton "Next Day ⇒" GoToNextDay
            ]
          ]
    True ->
      h1 [ class "flex justify-center flex-column" ] 
        [ label [ class "tc" ] 
          [ div [ ] [ text "Pick a day with sweeps:" ]
          , select [ onInput SelectDay ] 
              (case model.days of
                RemoteData.Success days ->
                  days
                  |> List.sortBy (\d -> Time.posixToMillis d.id )
                  |> List.map (\d ->
                    let
                      idString = 
                        d.id
                        |> posixToString
                      isSelected =
                        idString == Maybe.withDefault "" model.selectedDay
                    in
                    option [ value idString, selected isSelected ] [ text <| viewTime d.id model.zone ]
                    )
                _ -> []
              )
            ]
        , div [ class "flex justify-center" ]
          [ button [ onClick ChangeDay, class "btn" ] [ text "✔ Accept" ]
          , button [ onClick CancelChangeDay, class "btn" ] [ text "❌ Cancel" ]
          ]
        ]

viewSweepFile : String -> Maybe SM.SweepsFile -> Html Msg
viewSweepFile label sweepsFile  =
  case sweepsFile of
  Nothing -> text ""
  Just file ->
    a [ href file.url ] [ text <| "Download " ++ label ++ " pdf: " ++ file.name ]

viewSweeps : Bool -> Time.Zone -> RemoteData.WebData SM.Sweeps -> Html Msg
viewSweeps showUnconfirmed zone sweeps =
  case sweeps of
      RemoteData.NotAsked ->
        div [ class "tc" ] [ text "Loading" ]
      RemoteData.Loading ->
        div [ class "tc" ] [ text "Loading" ]
      RemoteData.Success sweep ->
          case sweep.activities of
            [] -> div [ class "tc" ] [ text "No sweeps loaded for today." ]
            _ ->
              let
                dateString =
                  getDate sweep.date zone
                csvUrl =
                  "/api/csv/" ++ dateString
              in
              div [ ] 
                [ p [] 
                  [ viewSweepFile "Confirmed Sweeps" sweep.currentFile
                  , viewSweepFile "Pending Sweeps" sweep.futureFile
                  , a [ class "mh2",  href csvUrl ] [ text <| "Download csv" ]
                  ]
                , viewFilter showUnconfirmed
                , table [] [
                  thead [] [
                    tr []
                      [ th [ class "w5" ] [ text "Address" ]
                      , th [] [ text "Division" ]
                      , th [] [ text "Location" ]
                      , th [] [ text "Action Type" ]
                      , th [] [ text "Comments" ]
                      , th [] [ text "Status" ]
                      ]
                  ],
                  tbody []
                    (sweep.activities
                    |> List.filter (\a ->
                      case showUnconfirmed of
                      True -> True
                      False ->
                        case a of
                        SM.Future _ -> False
                        _ -> True
                    )
                    |> List.map viewActivity)
                  ]
                ]
      RemoteData.Failure error ->
        div [ class "tc" ] [ text <| "Error: " ++ (errorToString error) ]

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

viewFilter showUnconfirmed =
  label [] 
    [ text "Show Unconfirmed Sweeps"
    , input [ type_ "checkbox", checked showUnconfirmed, onCheck SetShowUnconfirmed, class "ml3" ] []
    ]

view model =
  div []
    [ viewDatePicker model
    , viewSweeps model.showUnconfirmed model.zone model.sweeps
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