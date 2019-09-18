module DayModels exposing (..)

import Time
import Json.Decode as Decode exposing (Decoder, int, string)
import Json.Decode.Extra as DecodeExtra
import Json.Decode.Pipeline exposing (required, optional, hardcoded)

type alias Day = 
  { id: Time.Posix
  }

posixDecoder: Decoder Time.Posix
posixDecoder =
  Decode.int
  |> Decode.andThen (\i -> 
    i
    |> Time.millisToPosix
    |> Decode.succeed
  )

dayDecoder: Decoder Day
dayDecoder =
  Decode.succeed Day
  |> required "id" posixDecoder