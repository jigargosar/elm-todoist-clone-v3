module Drag exposing (..)

import Browser.Dom as Dom
import Browser.Events as BE
import Json.Decode as JD


type alias XY =
    { x : Float, y : Float }


type Drag
    = NotDragging
    | DragStartPending { domId : String, startXY : XY, currentXY : XY }
    | Dragging { domId : String, startXY : XY, currentXY : XY, dragElement : Dom.Element }


type Msg
    = GlobalMouseMove
    | GlobalMouseUp


subscriptions drag =
    let
        mouseSubscriptions =
            Sub.batch
                [ BE.onMouseMove <| JD.succeed GlobalMouseMove
                , BE.onMouseUp <| JD.succeed GlobalMouseUp
                ]
    in
    case drag of
        NotDragging ->
            Sub.none

        DragStartPending _ ->
            mouseSubscriptions

        Dragging _ ->
            mouseSubscriptions
