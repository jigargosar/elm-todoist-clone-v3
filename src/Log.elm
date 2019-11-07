port module Log exposing (..)

import Json.Decode as JD


port logError : String -> Cmd msg


logDecodeError : JD.Error -> Cmd msg
logDecodeError =
    logError << JD.errorToString
