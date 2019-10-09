module Timestamp exposing (Timestamp, decoder, zero)

import Json.Decode as JD exposing (Decoder)
import Time exposing (Posix)


type alias Timestamp =
    Posix


zero : Posix
zero =
    Time.millisToPosix 0


decoder : Decoder Timestamp
decoder =
    JD.int |> JD.map Time.millisToPosix
