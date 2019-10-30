module Focus exposing (FocusResult, attempt)

import Browser.Dom as Dom
import Task


type alias FocusResult =
    Result Dom.Error ()


attempt : String -> (FocusResult -> msg) -> Cmd msg
attempt domId msg =
    Dom.focus domId |> Task.attempt msg
