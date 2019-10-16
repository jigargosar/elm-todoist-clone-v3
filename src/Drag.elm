module Drag exposing (..)

import Browser.Dom as Dom exposing (Element)
import Browser.Events as BE
import Json.Decode as JD


type alias XY =
    { x : Float, y : Float }


type Drag
    = NotDragging
    | DragStartPending
        { dragId : String
        , startXY : XY
        , currentXY : XY
        }
    | Dragging
        { dragId : String
        , startXY : XY
        , currentXY : XY
        , dragElement : Dom.Element
        }
    | DraggingOver
        { dragId : String
        , startXY : XY
        , currentXY : XY
        , dragElement : Dom.Element
        , dropId : String
        , dropElement : Element
        }


type Msg
    = GlobalMouseMove
    | GlobalMouseUp
    | MouseDownOnDragZone String
    | MouseOverDropZone String
    | GotDragElement Element
    | GotDropElement Element
    | GotDomError Dom.Error


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
