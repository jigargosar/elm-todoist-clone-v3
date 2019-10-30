port module Focus exposing (FocusResult, attempt, logError, logIfError, onFocusOrClickOutSide)

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


logIfError : FocusResult -> Cmd msg
logIfError result =
    case result of
        Ok () ->
            Cmd.none

        Err error ->
            logError error


port onFocusOrClickOutSide : String -> Cmd msg
