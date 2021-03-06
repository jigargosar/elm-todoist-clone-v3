port module Focus exposing (FocusResult, attempt, cmd, logError, logIfError, onFocusOrClickOutside, registerOnFocusOrClickOutSide, unRegisterOnFocusOrClickOutSide)

import Basics.More exposing (flip)
import Browser.Dom as Dom
import Log
import Task


type alias FocusResult =
    Result Dom.Error ()


attempt : String -> (FocusResult -> msg) -> Cmd msg
attempt domId msg =
    Dom.focus domId |> Task.attempt msg


cmd : (FocusResult -> msg) -> String -> Cmd msg
cmd =
    flip attempt


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


port registerOnFocusOrClickOutSide : String -> Cmd msg


port unRegisterOnFocusOrClickOutSide : String -> Cmd msg


port onFocusOrClickOutside : (String -> msg) -> Sub msg
