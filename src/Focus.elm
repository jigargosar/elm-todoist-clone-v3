module Focus exposing (FocusResult, attempt, logError)

import Browser.Dom as Dom
import Log
import Task


type alias FocusResult =
    Result Dom.Error ()


attempt : String -> (FocusResult -> msg) -> Cmd msg
attempt domId msg =
    Dom.focus domId |> Task.attempt msg


logError : Dom.Error -> Cmd msg
logError (Dom.NotFound domId) =
    Log.logError <| "focus failed: " ++ domId
