module Cmds exposing (..)

import Basics.More exposing (msgToCmd)
import Browser.Dom as Dom
import Task


none : Cmd msg
none =
    Cmd.none


focus : String -> (Result Dom.Error () -> msg) -> Cmd msg
focus id msg =
    Dom.focus id
        |> Task.attempt msg


fromMsg : a -> Cmd a
fromMsg =
    msgToCmd


fromMaybe : Maybe (Cmd msg) -> Cmd msg
fromMaybe =
    Maybe.withDefault none


fromMaybeMsg =
    Maybe.map fromMsg >> fromMaybe
