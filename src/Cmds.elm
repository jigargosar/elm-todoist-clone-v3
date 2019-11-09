module Cmds exposing (..)

import Browser.Dom as Dom
import Task


none : Cmd msg
none =
    Cmd.none


focus : String -> (Result Dom.Error () -> msg) -> Cmd msg
focus id msg =
    Dom.focus id
        |> Task.attempt msg
