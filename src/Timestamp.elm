module Timestamp exposing (Timestamp, zero)

import Time exposing (Posix)


type alias Timestamp =
    Posix


zero : Posix
zero =
    Time.millisToPosix 0
