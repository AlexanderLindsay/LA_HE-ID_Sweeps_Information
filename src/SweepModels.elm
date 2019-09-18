module SweepModels exposing (..)

import Time
import Json.Decode as Decode exposing (Decoder, int, string)
import Json.Decode.Extra as DecodeExtra
import Json.Decode.Pipeline exposing (required, optional, hardcoded)

type SubLocation
  = Street
  | Underpass
  | Alley
  | OtherSubLocation String

subLocationFromString string =
  case string of
  "street" -> 
    Street
  "underpass" ->
    Underpass
  "alley" ->
    Alley
  somethingElse ->
    OtherSubLocation somethingElse
      
type LocationType
  = WithSidewalk SubLocation
  | NoSidewalk SubLocation
  | JustSidewalk
  | OtherLocation String

locationTypeDecoder: Decoder LocationType
locationTypeDecoder =
  Decode.string
  |> Decode.andThen (\str ->
    let
      segments = 
        String.split "/" str
        |> List.filter (\s -> s /= "")
        |> List.map String.toLower
    in
      case segments of
        [ "sidewalk" ] ->
          Decode.succeed JustSidewalk
        [ "sidewalk", subloc ] ->
          let
            sl = subLocationFromString subloc
          in
            Decode.succeed <| WithSidewalk sl
        [ subloc ] ->
          Decode.succeed <| NoSidewalk <| subLocationFromString subloc
        _ ->
          Decode.succeed <| OtherLocation str
  )

sublocationToStr subLocation =
  case subLocation of
    Street -> "Street"
    Underpass -> "Underpass"
    Alley -> "Alley"
    OtherSubLocation name -> name

locationToStr location =
  case location of
    WithSidewalk sl ->
      "Sidewalk/" ++ sublocationToStr sl
    NoSidewalk sl ->
      sublocationToStr sl
    JustSidewalk ->
      "Sidewalk"
    OtherLocation l -> l

type Division
  = Newton
  | WestValley
  | Harbor
  | Rampart
  | Olympic
  | Central
  | Hollenbeck
  | SeventySevenStreet
  | Southeast
  | Northeast
  | Wilshire
  | WestLosAngeles
  | NorthHollywood
  | OtherDivision String
  
divisionToStr division =
  case division of
  Newton -> "Newton"
  WestValley -> "West Valley"
  Harbor -> "Harbor"
  Rampart -> "Rampart"
  Olympic -> "Olympic"
  Central -> "Central"
  SeventySevenStreet -> "77th Street"
  Hollenbeck -> "Hollenbeck"
  Southeast -> "Southeast"
  Northeast -> "Northeast"
  Wilshire -> "Wilshire"
  WestLosAngeles -> "West Los Angeles"
  NorthHollywood -> "North Hollywood"
  OtherDivision name -> name

divisionDecoder: Decoder Division
divisionDecoder =
  Decode.string
  |> Decode.andThen (\str ->
    let
      lower = String.toLower str
    in
    case lower of
    "newton" -> 
      Decode.succeed Newton
    "west valley" ->
      Decode.succeed WestValley
    "harbor" ->
      Decode.succeed Harbor
    "rampart" ->
      Decode.succeed Rampart
    "olympic" ->
      Decode.succeed Olympic
    "central" ->
      Decode.succeed Central
    "hollenbeck" ->
      Decode.succeed Hollenbeck
    "77th street" ->
      Decode.succeed SeventySevenStreet
    "southeast" ->
      Decode.succeed Southeast
    "northeast" ->
      Decode.succeed Northeast
    "wilshire" ->
      Decode.succeed Wilshire
    "west los angeles" ->
      Decode.succeed WestLosAngeles
    "north hollywood" ->
      Decode.succeed NorthHollywood
    somethingElse ->
      Decode.succeed <| OtherDivision str
  )

type Status
  = Approved
  | MissingLAHSASignature
  | OtherStatus String
  
statusToStr status =
  case status of
  Approved -> 
    "Approved"
  MissingLAHSASignature ->
    "Missing LAHSA Signature"
  OtherStatus str -> str

statusDecoder: Decoder Status
statusDecoder =
  Decode.string
  |> Decode.andThen (\str ->
    let
      lower = String.toLower str
    in
    case lower of
    "approved" -> 
      Decode.succeed Approved
    "missing lahsa signature" ->
      Decode.succeed MissingLAHSASignature
    s -> 
      Decode.succeed <| OtherStatus s
  )

type alias DivisionActivity =
  { authNumber: String
  , address: String
  , crossStreetOne: String
  , crossStreetTwo: String
  , location: Maybe LocationType
  , comments: String
  , cleaningTime: String
  , division: Maybe Division
  , status: Status
  }

divisionActivityDecoder: Decoder DivisionActivity
divisionActivityDecoder =
  Decode.succeed DivisionActivity
  |> required "authNumber" string
  |> required "address" string
  |> required "crossStreetOne" string
  |> required "crossStreetTwo" string
  |> optional "location" (Decode.map Just locationTypeDecoder) Nothing
  |> required "comments" string
  |> required "cleaningTime" string
  |> optional "division" (Decode.map Just divisionDecoder) Nothing
  |> required "status" statusDecoder

type MaintenanceStatus
  = IllegalDump
  | OtherMaintenanceStatus String

maintenanceStatusToStr status =
  case status of
  IllegalDump -> "Illegal Dump"
  OtherMaintenanceStatus other -> other

maintenanceStatusDecoder: Decoder MaintenanceStatus
maintenanceStatusDecoder =
  Decode.string
  |> Decode.andThen (\str ->
    let
      lower = String.toLower str
    in
    case lower of
    "IllegalDump" ->
      Decode.succeed IllegalDump
    other ->
      Decode.succeed <| OtherMaintenanceStatus str
  )

type alias MaintenanceActivity =
  { idTeam: String
  , address: String
  , location: Maybe LocationType
  , comments: String
  , division: Maybe Division
  , status: MaintenanceStatus
  }

maintenanceActivityDecoder : Decoder MaintenanceActivity
maintenanceActivityDecoder =
  Decode.succeed MaintenanceActivity
  |> required "idTeam" string
  |> required "address" string
  |> optional "location" (Decode.map Just locationTypeDecoder) Nothing
  |> required "comments" string
  |> optional "division" (Decode.map Just divisionDecoder) Nothing
  |> required "status" maintenanceStatusDecoder
  
type Activity
  = Maintenance MaintenanceActivity
  | Division DivisionActivity
  
activityDecoder : Decoder Activity
activityDecoder =
  Decode.oneOf
    [ Decode.map Maintenance maintenanceActivityDecoder
    , Decode.map Division divisionActivityDecoder
    ]
  
type alias Sweeps =
  { date: Time.Posix
  , activities: List Activity
  , url: String
  , name: String
  }

sweepsDecoder : Decoder Sweeps
sweepsDecoder =
  Decode.succeed Sweeps
  |> required "date" DecodeExtra.datetime
  |> required "activities" (Decode.list activityDecoder)
  |> required "url" string
  |> required "name" string