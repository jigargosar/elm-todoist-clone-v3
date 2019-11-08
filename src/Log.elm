port module Log exposing (..)

import Browser.Dom as Dom
import Json.Decode as JD


port logError : String -> Cmd msg


logDecodeError : JD.Error -> Cmd msg
logDecodeError =
    logError << JD.errorToString


focusError : Dom.Error -> Cmd msg
focusError (Dom.NotFound domId) =
    logError <| "autofocus failed: " ++ domId
